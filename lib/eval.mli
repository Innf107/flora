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
  | IncorrectNumberOfArgsToContinuation of int
  | PrimopArgumentError of {
      primop : primop;
      expected : string;
      actual : value list;
    }
  | NonexhaustivePatterns of { scrutinee : value }

exception EvalError of loc * eval_error

type ('a, 'r) cont =
  | Done : ('r, 'r) cont
  | EvalAppFun : loc * env * expr list * (value, 'r) cont -> (value, 'r) cont
  | EvalAppArgs :
      (env * pattern list * expr)
      * env
      * value list
      * expr list
      * (value, 'r) cont
      -> (value, 'r) cont
  | EvalAppPrimop :
      primop * env * loc * value list * expr list * (value, 'r) cont
      -> (value, 'r) cont
  | IfCont : loc * env * expr * expr * (value, 'r) cont -> (value, 'r) cont
  | WithEnv : env * (env * value, 'r) cont -> (value, 'r) cont
  | IgnoreEnv : (value, 'r) cont -> (env * value, 'r) cont
  | EvalSequence :
      env * statement list * (env * value, 'r) cont
      -> ('a, 'r) cont
  | BindValue :
      env * pattern * statement list * (env * value, 'r) cont
      -> (value, 'r) cont
  | StrictBinOp1 :
      loc * env * strict_binop * expr * (value, 'r) cont
      -> (value, 'r) cont
  | StrictBinOp2 :
      loc * value * strict_binop * (value, 'r) cont
      -> (value, 'r) cont
  | LazyBinOp :
      loc * env * lazy_binop * expr * (value, 'r) cont
      -> (value, 'r) cont
  | PerformArgs :
      loc * env * name * expr list * value list * (value, 'r) cont
      -> (value, 'r) cont
  | Compose : ('a, 'b) cont * ('b, 'c) cont -> ('a, 'c) cont
  | EvalListLiteral :
      env * value list * expr list * (value, 'r) cont
      -> (value, 'r) cont
  | EvalRecordLiteral :
      env * name * value RecordMap.t * (name * expr) list * (value, 'r) cont
      -> (value, 'r) cont
  | EvalMatch :
      loc * env * (pattern * expr) list * (value, 'r) cont
      -> (value, 'r) cont

type 'r eval_result =
  | Completed of 'r
  | Suspended of name * value list * (value, 'r) cont

val eval : env -> expr -> value eval_result
val eval_statements : env -> statement list -> (env * value) eval_result
val continue : ('a, 'r) cont -> 'a -> 'r eval_result
