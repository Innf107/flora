open Util

type loc = Loc.t

type lexical_error =
  | UnexpectedChar of char * loc
  | UnexpectedEOF
  | UnterminatedString

exception LexicalError of lexical_error

type lex_state = {
  filename : string;
  buffer : Bytes.t;
  mutable buffer_index : int;
  mutable start_pos : Lexing.position;
  mutable end_pos : Lexing.position;
}

let current_loc lex_state =
  let start_line = lex_state.start_pos.pos_lnum in
  let start_column =
    lex_state.start_pos.pos_cnum - lex_state.start_pos.pos_bol + 1
  in
  let end_line = lex_state.end_pos.pos_lnum in
  let end_column = lex_state.end_pos.pos_cnum - lex_state.end_pos.pos_bol + 1 in
  Loc.
    {
      file = lex_state.filename;
      start_line;
      start_column;
      end_line;
      end_column;
    }

let unexpected state = function
  | None -> raise (LexicalError UnexpectedEOF)
  | Some char -> raise (LexicalError (UnexpectedChar (char, current_loc state)))

let peek_char : lex_state -> char option =
 fun state ->
  if state.buffer_index >= Bytes.length state.buffer then None
  else
    let char = Bytes.get state.buffer state.buffer_index in
    Some char

let peek_char2 : lex_state -> char option =
 fun state ->
  if state.buffer_index + 1 >= Bytes.length state.buffer then None
  else
    let char = Bytes.get state.buffer (state.buffer_index + 1) in
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
let indentation_at pos = Lexing.(pos.pos_cnum - pos.pos_bol)

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

let is_alpha = function
  | 'a' .. 'z'
  | 'A' .. 'Z' ->
      true
  | _ -> false

let is_digit = function
  | '0' .. '9' -> true
  | _ -> false

let is_ident_start char = is_alpha char || char = '_'
let is_ident char = is_alpha char || is_digit char || char = '_'
let string_of_reverse_list list = String.of_seq (List.to_seq (List.rev list))

let rec lex state =
  let advance_emit (token : Parser.token) =
    advance state;
    token
  in
  match peek_char state with
  | None -> Parser.EOF
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
          begin
            match peek_char state with
            | None -> raise (LexicalError UnterminatedString)
            | Some '"' ->
                advance state;
                begin
                  match peek_char state with
                  | Some '"' ->
                      advance state;
                      lex_triple_string [] state
                  | None
                  | Some _ ->
                      STRING ""
                end
                (* This is a regular string literal so we
                   don't advance and let lex_string take over *)
            | Some _ -> lex_string [] state
          end
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
            | _ -> MINUS
          end
      | '*' -> advance_emit STAR
      | '/' -> advance_emit SLASH
      | '<' ->
          advance state;
          begin
            match peek_char state with
            | Some '=' -> advance_emit LESSEQUAL
            | _ -> LESS
          end
      | '=' ->
          advance state;
          begin
            match peek_char state with
            | Some '=' -> advance_emit DOUBLEEQUAL
            | _ -> EQUALS
          end
      | '!' ->
          advance state;
          begin
            match peek_char state with
            | Some '=' -> advance_emit NOTEQUAL
            | other -> unexpected state other
          end
      | '>' ->
          advance state;
          begin
            match peek_char state with
            | Some '=' -> advance_emit GREATEREQUAL
            | _ -> GREATER
          end
      | ':' ->
          advance state;
          begin
            match peek_char state with
            | Some ':' -> advance_emit DOUBLECOLON
            | other -> unexpected state other
          end
      | '~' -> advance_emit TILDE
      | '&' ->
          advance state;
          begin
            match peek_char state with
            | Some '&' -> advance_emit AND
            | other -> unexpected state other
          end
      | '|' ->
          advance state;
          begin
            match peek_char state with
            | Some '|' -> advance_emit OR
            | other -> PIPE
          end
      | ',' -> advance_emit COMMA
      | char -> unexpected state (Some char))

and lex_line_comment state =
  match peek_char state with
  | None -> EOF
  | Some '\n' ->
      advance state;
      advance_whitespace state;
      lex state
  | _ ->
      advance state;
      advance_whitespace state;
      lex_line_comment state

and lex_ident accum state =
  match peek_char state with
  | Some char when is_ident char ->
      advance state;
      lex_ident (char :: accum) state
  | _ -> ident_of_string (string_of_reverse_list accum)

and lex_integer accum state =
  match peek_char state with
  | Some char when is_digit char ->
      advance state;
      lex_integer (char :: accum) state
  | Some '.' ->
      advance state;
      lex_float accum state
  | _ -> NUMBER (float_of_string (string_of_reverse_list accum))

and lex_float accum state =
  match peek_char state with
  | Some char when is_digit char ->
      advance state;
      lex_float (char :: accum) state
  | _ -> NUMBER (float_of_string (string_of_reverse_list accum))

and lex_string accum state =
  match peek_char state with
  | None -> raise (LexicalError UnterminatedString)
  | Some '"' ->
      advance state;
      STRING (string_of_reverse_list accum)
  | Some char ->
      advance state;
      lex_string (char :: accum) state

and lex_triple_string accum state =
  match peek_char state with
  | None -> raise (LexicalError UnterminatedString)
  | Some '"' ->
      advance state;
      begin
        match peek_char state with
        | None -> raise (LexicalError UnterminatedString)
        | Some '"' ->
            advance state;
            begin
              match peek_char state with
              | Some '"' ->
                  advance state;
                  STRING (string_of_reverse_list accum)
              | None -> raise (LexicalError UnterminatedString)
              | Some char ->
                  advance state;
                  lex_triple_string (char :: '"' :: '"' :: accum) state
            end
        | Some char ->
            advance state;
            lex_triple_string (char :: '"' :: accum) state
      end
  | Some char ->
      advance state;
      lex_triple_string (char :: accum) state

let run ~filename string =
  let state : lex_state =
    {
      filename;
      buffer = String.to_bytes string;
      buffer_index = 0;
      start_pos =
        Lexing.{ pos_fname = filename; pos_lnum = 0; pos_bol = 0; pos_cnum = 1 };
      end_pos =
        Lexing.{ pos_fname = filename; pos_lnum = 0; pos_bol = 0; pos_cnum = 1 };
    }
  in
  let initial_token = ref true in
  fun () ->
    let token =
      if !initial_token then begin
        initial_token := false;
        lex state
      end
      else lex state
    in
    (token, state.start_pos, state.end_pos)
