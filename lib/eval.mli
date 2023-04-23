open Syntax

type eval_error =
    | VarNotFound of string
    | TryingToCallNonFunction of value
    | IncorrectNumberOfArgs of { expected : int; actual : int }

exception EvalError of loc * eval_error

val eval : env -> expr -> env * value
