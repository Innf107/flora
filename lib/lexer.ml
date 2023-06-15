open Util

type lexical_error =
  | UnexpectedChar of char
  | UnexpectedEOF
  | UnterminatedString
  | TooManyClosedBlocks

exception LexicalError of lexical_error

let unexpected = function
  | None -> raise (LexicalError UnexpectedEOF)
  | Some char -> raise (LexicalError (UnexpectedChar char))

type indentation_state =
  | Opening
  | Found of int

type lex_state = {
  buffer : Bytes.t;
  mutable buffer_index : int;
  mutable start_pos : Lexing.position;
  mutable end_pos : Lexing.position;
  mutable block_indentation : indentation_state list;
}

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

let open_block state =
  state.block_indentation <- Opening :: state.block_indentation

let close_block state =
  match state.block_indentation with
  | _ :: rest -> state.block_indentation <- rest
  | [] -> raise (LexicalError TooManyClosedBlocks)

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
      | '\n' ->
          advance state;
          advance_whitespace state;
          lex_leading_whitespace state
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
      | '{' ->
          advance state;
          open_block state;
          LBRACE
      | '}' ->
          advance state;
          close_block state;
          RBRACE
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
            | other -> unexpected other
          end
      | '>' ->
          advance state;
          begin
            match peek_char state with
            | Some '=' -> advance_emit GREATEREQUAL
            | _ -> GREATER
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
            | other -> PIPE
          end
      | ';' -> advance_emit SEMI
      | ',' -> advance_emit COMMA
      | char -> raise (LexicalError (UnexpectedChar char)))

and lex_leading_whitespace state =
  match (peek_char state, peek_char2 state) with
  | Some char, _ when is_whitespace char ->
      advance state;
      advance_whitespace state;
      lex_leading_whitespace state
  (* Comments should not have an impact on layout *)
  | Some '-', Some '-' ->
      advance state;
      advance state;
      advance_whitespace state;
      lex_line_comment state
  | _ -> begin
      match state.block_indentation with
      | [] -> raise (LexicalError TooManyClosedBlocks)
      | Opening :: rest ->
          state.block_indentation <-
            Found (indentation_at state.start_pos) :: rest;
          lex state
      | Found indentation :: _ ->
          if indentation_at state.start_pos <= indentation then SEMI
          else lex state
    end

and lex_line_comment state =
  match peek_char state with
  | None -> EOF
  | Some '\n' ->
      advance state;
      advance_whitespace state;
      lex_leading_whitespace state
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

let run ~filename string =
  let state : lex_state =
    {
      buffer = String.to_bytes string;
      buffer_index = 0;
      start_pos =
        Lexing.{ pos_fname = filename; pos_lnum = 0; pos_bol = 0; pos_cnum = 1 };
      end_pos =
        Lexing.{ pos_fname = filename; pos_lnum = 0; pos_bol = 0; pos_cnum = 1 };
      block_indentation = [ Opening ];
    }
  in
  let initial_token = ref true in
  fun () ->
    let token =
      if !initial_token then begin
        initial_token := false;
        lex_leading_whitespace state
      end
      else lex state
    in
    (token, state.start_pos, state.end_pos)
