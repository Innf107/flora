open Syntax

type eval_error =
  | VarNotFound of string
  | TryingToCallNonFunction of value
  | IncorrectNumberOfArgs of {
      expected : int;
      actual : int;
    }
  | InvalidOperatorArgs of {
      operator : string;
      expected : string list;
      actual : value option list;
    }
  | IncorrectNumberOfHandlerArgs of {
      effect : name;
      expected : int;
      actual : int;
    }

exception EvalError of loc * eval_error

type ('a, 'r) cont

type 'r eval_result =
  | Completed of 'r
  | Suspended of name * value list * (value, 'r) cont

val eval : env -> expr -> value eval_result
val eval_statements : env -> statement list -> (env * value) eval_result
