open Util

type loc = Loc.t

type lexical_error =
  | UnexpectedChar of char * loc
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
  | "\"\"\"", Star any, "\"\"\"" ->
      STRING
        (Sedlexing.Utf8.sub_lexeme lexbuf 1
           (Sedlexing.lexeme_length lexbuf - 2))
  | "\"\"\"", Star any, eof -> raise (LexicalError UnterminatedString)
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
  | any -> raise (LexicalError (UnexpectedChar ((Sedlexing.Utf8.sub_lexeme lexbuf 0 1).[0], current_loc lexbuf)))
  | _ -> assert false
