open Util
open Effect
open Effect.Deep

type lexical_error =
  | UnexpectedChar of char
  | UnexpectedEOF
  | UnterminatedString

exception LexicalError of lexical_error

let unexpected = function
  | None -> raise (LexicalError UnexpectedEOF)
  | Some char -> raise (LexicalError (UnexpectedChar char))

type lex_state = {
  buffer : Bytes.t;
  mutable buffer_index : int;
  mutable start_pos : Lexing.position;
  mutable end_pos : Lexing.position;
}

type _ Effect.t +=
  | Emit : Parser.token * Lexing.position * Lexing.position -> unit Effect.t

let emit state token =
  perform (Emit (token, state.start_pos, state.end_pos));
  state.start_pos <- state.end_pos

let peek_char : lex_state -> char option =
 fun state ->
  if state.buffer_index >= Bytes.length state.buffer then None
  else
    let char = Bytes.get state.buffer state.buffer_index in
    Some char

let advance : lex_state -> unit =
 fun state ->
  if state.buffer_index < Bytes.length state.buffer then begin
    begin
      match Bytes.get state.buffer state.buffer_index with
      | '\n' ->
          state.end_pos <-
            {
              state.end_pos with
              pos_lnum = state.end_pos.pos_lnum + 1;
              pos_bol = state.end_pos.pos_cnum;
              pos_cnum = state.end_pos.pos_cnum + 1;
            }
      | _ ->
          state.end_pos <-
            { state.end_pos with pos_cnum = state.end_pos.pos_cnum + 1 }
    end;
    state.buffer_index <- state.buffer_index + 1
  end

let advance_whitespace state : unit = state.start_pos <- state.end_pos

let next_char state =
  let char = peek_char state in
  advance state;
  char

let ident_of_string = function
  | "let" -> Parser.LET
  | "if" -> Parser.IF
  | "then" -> Parser.THEN
  | "else" -> Parser.ELSE
  | "nil" -> Parser.NIL
  | "true" -> Parser.TRUE
  | "false" -> Parser.FALSE
  | "perform" -> Parser.PERFORM
  | "handle" -> Parser.HANDLE
  | "match" -> Parser.MATCH
  | "as" -> Parser.AS
  | ident -> Parser.IDENT ident

let is_whitespace char = String.contains " \t\n" char
let is_alpha = function 'a' .. 'z' | 'A' .. 'Z' -> true | _ -> false
let is_digit = function '0' .. '9' -> true | _ -> false
let is_ident_start char = is_alpha char || char = '_'
let is_ident char = is_alpha char || is_digit char || char = '_'
let string_of_reverse_list list = String.of_seq (List.to_seq (List.rev list))

(* We use a custom result here instead of unit to ensure that
   lexing functions (except 'lex') always tail call each other and don't
   accidentally stop in the middle of lexing *)
type lex_completion = Completed

let rec lex state =
  let advance_emit token =
    advance state;
    emit state token;
    lex state
  in
  match peek_char state with
  | None -> Completed
  | Some char -> (
      match char with
      | char when is_whitespace char ->
          advance state;
          advance_whitespace state;
          lex state
      | char when is_ident_start char ->
          advance state;
          lex_ident [ char ] state
      | char when is_digit char ->
          advance state;
          lex_integer [ char ] state
      | '"' ->
          advance state;
          lex_string [] state
      | '\\' -> advance_emit LAMBDA
      | '(' -> advance_emit LPAREN
      | ')' -> advance_emit RPAREN
      | '{' -> advance_emit LBRACE
      | '}' -> advance_emit RBRACE
      | '[' -> advance_emit LBRACKET
      | ']' -> advance_emit RBRACKET
      | '+' -> advance_emit PLUS
      | '-' ->
          advance state;
          begin
            match peek_char state with
            | Some '-' ->
                advance state;
                lex_line_comment state
            | Some '>' -> advance_emit ARROW
            | _ ->
                emit state MINUS;
                lex state
          end
      | '*' -> advance_emit STAR
      | '/' -> advance_emit SLASH
      | '<' ->
          advance state;
          begin
            match peek_char state with
            | Some '=' -> advance_emit LESSEQUAL
            | _ ->
                emit state LESS;
                lex state
          end
      | '=' ->
          advance state;
          begin
            match peek_char state with
            | Some '=' -> advance_emit DOUBLEEQUAL
            | _ ->
                emit state EQUALS;
                lex state
          end
      | '!' ->
          advance state;
          begin
            match peek_char state with
            | Some '=' -> advance_emit NOTEQUAL
            | other -> unexpected other
          end
      | '>' ->
          advance state;
          begin
            match peek_char state with
            | Some '=' -> advance_emit GREATEREQUAL
            | _ ->
                emit state GREATER;
                lex state
          end
      | ':' -> advance_emit COLON
      | '~' -> advance_emit TILDE
      | '&' ->
          advance state;
          begin
            match peek_char state with
            | Some '&' -> advance_emit AND
            | other -> unexpected other
          end
      | '|' ->
          advance state;
          begin
            match peek_char state with
            | Some '|' -> advance_emit OR
            | other ->
                emit state PIPE;
                lex state
          end
      | ';' -> advance_emit SEMI
      | ',' -> advance_emit COMMA
      | char -> raise (LexicalError (UnexpectedChar char)))

and lex_line_comment state =
  match next_char state with
  | Some '\n' -> lex state
  | _ -> lex_line_comment state

and lex_ident accum state =
  match peek_char state with
  | Some char when is_ident char ->
      advance state;
      lex_ident (char :: accum) state
  | _ ->
      emit state (ident_of_string (string_of_reverse_list accum));
      lex state

and lex_integer accum state =
  match peek_char state with
  | Some char when is_digit char ->
      advance state;
      lex_integer (char :: accum) state
  | Some '.' ->
      advance state;
      lex_float accum state
  | _ ->
      emit state (NUMBER (float_of_string (string_of_reverse_list accum)));
      lex state

and lex_float accum state =
  match peek_char state with
  | Some char when is_digit char ->
      advance state;
      lex_float (char :: accum) state
  | _ ->
      emit state (NUMBER (float_of_string (string_of_reverse_list accum)));
      lex state

and lex_string accum state =
  match peek_char state with
  | None -> raise (LexicalError UnterminatedString)
  | Some '"' ->
      advance state;
      emit state (STRING (string_of_reverse_list accum));
      lex state
  | Some char ->
      advance state;
      lex_string (char :: accum) state

let run ~filename string =
  let state : lex_state =
    {
      buffer = String.to_bytes string;
      buffer_index = 0;
      start_pos =
        Lexing.{ pos_fname = filename; pos_lnum = 0; pos_bol = 0; pos_cnum = 0 };
      end_pos =
        Lexing.{ pos_fname = filename; pos_lnum = 0; pos_bol = 0; pos_cnum = 0 };
    }
  in
  let rec next =
    ref
      begin
        fun () ->
          match_with lex state
            {
              effc =
                (fun (type a) (eff : a Effect.t) ->
                  match eff with
                  | Emit (token, start_pos, end_pos) ->
                      Some
                        begin
                          fun (cont : (a, _) continuation) ->
                            next := continue cont;
                            (token, start_pos, end_pos)
                        end
                  | _ -> None);
              (* Returning means we exhausted the buffer *)
              retc =
                (fun Completed ->
                  (next := fun () -> (Parser.EOF, state.start_pos, state.end_pos));
                  (Parser.EOF, state.start_pos, state.end_pos));
              exnc = raise;
            }
      end
  in
  fun () -> !next ()
