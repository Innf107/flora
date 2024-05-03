open Util

type loc = Loc.t

type lexical_error =
  | UnexpectedChar of string * loc
  | UnexpectedEOF
  | UnterminatedString

exception LexicalError of lexical_error

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

let current_loc lexbuf =
  let start, end_ = Sedlexing.lexing_positions lexbuf in
  Loc.from_positions start end_

let rec lex lexbuf =
  let open Parser in
  let lexeme () = Sedlexing.Utf8.lexeme lexbuf in

  match%sedlex lexbuf with
  | Plus white_space -> lex lexbuf
  | (alphabetic | '_'), Star (alphabetic | '_' | 0 .. 9) ->
      ident_of_string (lexeme ())
  | "--", Star (Compl ('\n' | eof)) -> lex lexbuf
  | Opt '-', Plus '0' .. '9', Opt '.', Star '0' .. '9' ->
      NUMBER (float_of_string (lexeme ()))
  | "\"\"\"" ->
    let start_pos, _end_pos = Sedlexing.lexing_positions lexbuf in
    lex_triple_string lexbuf (Buffer.create 16) start_pos
  | '"', Star (Compl '"'), '"' ->
      STRING
        (Sedlexing.Utf8.sub_lexeme lexbuf 1
           (Sedlexing.lexeme_length lexbuf - 2))
  | '"', Star (Compl '"'), eof -> raise (LexicalError UnterminatedString)
  | "->" -> ARROW
  | "<=" -> LESSEQUAL
  | "==" -> DOUBLEEQUAL
  | "!=" -> NOTEQUAL
  | ">=" -> GREATEREQUAL
  | "::" -> DOUBLECOLON
  | "&&" -> AND
  | "||" -> OR
  | '\\' -> LAMBDA
  | '(' -> LPAREN
  | ')' -> RPAREN
  | '{' -> LBRACE
  | '}' -> RBRACE
  | '[' -> LBRACKET
  | ']' -> RBRACKET
  | '+' -> PLUS
  | '-' -> MINUS
  | '*' -> STAR
  | '/' -> SLASH
  | '<' -> LESS
  | '=' -> EQUALS
  | '>' -> GREATER
  | '~' -> TILDE
  | '|' -> PIPE
  | ',' -> COMMA
  | eof -> EOF
  | any ->
      raise
        (LexicalError
           (UnexpectedChar
              (lexeme (), current_loc lexbuf)))
  | _ -> assert false

and lex_triple_string lexbuf buffer startpos =
  match%sedlex lexbuf with
  | "\"\"\"" ->
    (* TODO: Somehow convince sedlex to set the start position to the correct value *)
    STRING (Buffer.contents buffer)
  | Plus (Compl '"')
  | '"' ->
      Buffer.add_string buffer (Sedlexing.Utf8.lexeme lexbuf);
      lex_triple_string lexbuf buffer startpos;
  | eof -> raise (LexicalError UnterminatedString)
  | _ -> assert false
