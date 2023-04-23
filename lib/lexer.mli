type lexical_error =
  | UnexpectedChar of char
  | UnexpectedEOF
  | UnterminatedString

exception LexicalError of lexical_error

type lex_state

val run :
  filename:string ->
  string ->
  unit ->
  Parser.token * Lexing.position * Lexing.position
