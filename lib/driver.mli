open Syntax

val eval_string : filename:string option -> env -> string -> (env * value) Eval.eval_result
