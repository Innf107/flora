type lexical_error =
  | UnexpectedChar of string * Loc.t
  | UnexpectedEOF
  | UnterminatedString

exception LexicalError of lexical_error

val lex : Sedlexing.lexbuf -> Parser.token
