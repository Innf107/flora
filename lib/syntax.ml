type name = string

module NameMap = Map.Make (String)

type loc = Loc.t

type literal =
  | NilLit
  | NumberLit of float
  | StringLit of string
  | BoolLit of bool

type strict_binop =
  [ `Add
  | `Subtract
  | `Multiply
  | `Divide
  | `Less
  | `LessOrEqual
  | `Equal
  | `NotEqual
  | `GreaterOrEqual
  | `Greater
  | `Cons
  | `Concat
  ]

type lazy_binop =
  [ `Or
  | `And
  ]

type binop =
  [ strict_binop
  | lazy_binop
  ]

type statement =
  | Let of loc * name * expr
  | LetFun of loc * name * name list * expr
  | RunExpr of expr

and expr =
  | Var of loc * name
  | App of loc * expr * expr list
  | Lambda of loc * name list * expr
  | Literal of loc * literal
  | ListLiteral of loc * expr list
  | Binop of loc * expr * binop * expr
  | If of loc * expr * expr * expr
  | Sequence of statement list
  | Perform of loc * name * expr list
  | Handle of loc * expr * (name * name list * name * expr) list

(* See Note [Continuation Representation]

   The type of continuations should be kept abstract to consumers of this module,
   but we don't have an mli file because of the type definitions.
   That's why we use this hack to keep continuations abstract to the outside *)
include (
  struct
    type continuation = Obj.t
  end :
    sig
      type continuation
    end)

type primop = DynamicVar

let parse_primop = function "dynamicVar" -> Some DynamicVar | _ -> None

type value =
  | Nil
  | Number of float
  | String of string
  | Bool of bool
  | List of value list
  (* The environment needs to be lazy to allow recursive definitions *)
  | Closure of env Lazy.t * name list * expr
  | Primop of primop
  (* See Note [Continuation Representation]*)
  | Continuation of continuation

and env = {
  contents : env_contents;
  (* See Note [Environment Provenance]*)
  previous : env option;
  delta : env_contents;
}

and env_contents = { variables : value NameMap.t }

let empty_env =
  {
    contents = { variables = NameMap.empty };
    previous = None;
    delta = { variables = NameMap.empty };
  }

let bind_variables bindings env =
  {
    contents =
      {
        env.contents with
        variables = NameMap.add_seq bindings env.contents.variables;
      };
    delta = { variables = NameMap.of_seq bindings };
    previous = Some env;
  }

let bind_variable name value env =
  bind_variables (List.to_seq [ (name, value) ]) env

let pretty_primop = function DynamicVar -> "dynamicVar"

let pretty_literal = function
  | NumberLit f -> string_of_float f
  | StringLit str -> "\"" ^ str ^ "\""
  | NilLit -> "nil"
  | BoolLit bool -> string_of_bool bool

let pretty_binop : binop -> string = function
  | `Add -> "+"
  | `Subtract -> "-"
  | `Multiply -> "*"
  | `Divide -> "/"
  | `Less -> "<"
  | `LessOrEqual -> "<="
  | `Equal -> "=="
  | `NotEqual -> "!="
  | `GreaterOrEqual -> ">="
  | `Greater -> ">"
  | `Cons -> ":"
  | `Concat -> "~"
  | `Or -> "||"
  | `And -> "&&"

let rec pretty_value = function
  | Number f -> string_of_float f
  | String str -> "\"" ^ str ^ "\""
  | Bool bool -> string_of_bool bool
  | List list -> "[" ^ String.concat ", " (List.map pretty_value list) ^ "]"
  | Nil -> "nil"
  | Closure (_, params, _) ->
      "<[closure](" ^ String.concat ", " params ^ ") -> ...>"
  | Primop _ -> "<primitive closure>"
  | Continuation _ -> "<continuation ...>"

(* Note [Environment Provenance]

   For performance reasons, environments are represented as flat key-value maps.
   While convenient, this makes serialization quite difficult.
   Every closure contains its defining environment, which barely differs from the one
   it was derived from.
   If we were to serialize these naively, we would duplicate the vast majority of the
   environment for every single closure, which would lead to a quadratic size explosion.

   To avoid this, every environment carries information about which previous environment it
   was derived from and what has changed between them.
   While this might increase memory usage quite a bit, it has the advantage of making
   serialization orders of magnitude simpler.
   On disk, serialized environments only contain information about how they were derived
   with explicit references to the previous environment. The deserialization process then takes that
   information and collects the environments into flat maps that the evaluator can use
   with all the sharing it needs.
*)

(* Note [Continuation Representation]

   Continuations should be represented by the type (value, value) Eval.cont.
   Unfortunately, Eval depends on this module, so we cannot actually include that type here
   thanks to OCaml's lack of mutually recursive modules without explicit signatures.
   Fortunately for us, the representation of continuations should be kept abstract anyway,
   so we can cheat.
   We cast continuations to Obj.t (equivalent to e.g. void* in C or Any in Haskell) to avoid any mutual recursion.
   Eval then casts those back to (value, value) cont values to process them.
   This gives up a bit of type safety, so we need to make absolutely sure that Continuation values
   never store anything other than actual continuations or we will end up with segmentation faults.
*)
