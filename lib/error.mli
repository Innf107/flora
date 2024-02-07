type t =
  | LexicalError of Lexer.lexical_error
  | ParseError of Loc.t
  | EvalError of Loc.t * Eval.eval_error
  | DeserializationError of Serialize.deserialization_error

val handle : handler:(t -> 'a) -> (unit -> 'a) -> 'a
val pretty : t -> string
