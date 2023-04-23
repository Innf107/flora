type t =
  | LexicalError of Lexer.lexical_error
  | ParseError (* TODO: Carry the source location of the parse error *)
  | EvalError of Loc.t * Eval.eval_error

val handle : handler:(t -> 'a) -> (unit -> 'a) -> 'a
val pretty : t -> string
