open Syntax

exception ParseError of Loc.t

val eval_string : filename:string option -> env -> string -> (env * value) Eval.eval_result
