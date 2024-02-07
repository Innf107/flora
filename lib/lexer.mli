type lexical_error =
  | UnexpectedChar of char * Loc.t
  | UnexpectedEOF
  | UnterminatedString

exception LexicalError of lexical_error

val lex : Sedlexing.lexbuf -> Parser.token
