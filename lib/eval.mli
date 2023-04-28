open Syntax

type eval_error =
    | VarNotFound of string
    | TryingToCallNonFunction of value
    | IncorrectNumberOfArgs of { expected : int; actual : int }
    | InvalidOperatorArgs of {
        operator : string;
        expected : string list;
        actual : value option list;
      }
    
exception EvalError of loc * eval_error

val eval : env -> expr -> value

val eval_statements : env -> statement list -> env * value
