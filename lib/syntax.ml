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
  | Binop of loc * expr * binop * expr
  | If of loc * expr * expr * expr
  | Sequence of statement list

and value =
  | Nil
  | Number of float
  | String of string
  | Bool of bool
  | List of value list
  (* The environment needs to be lazy to allow recursive definitions *)
  | Closure of env Lazy.t * name list * expr

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

let rec pretty_expr = function
  | Var (_, name) -> name
  | App (_, fun_expr, args) ->
      pretty_expr fun_expr ^ "("
      ^ String.concat ", " (List.map pretty_expr args)
      ^ ")"
  | Lambda (_, parameters, body) ->
      "(\\" ^ String.concat ", " parameters ^ " -> " ^ pretty_expr body ^ ")"
  | Literal (_, literal) -> pretty_literal literal
  | Binop (_, left, op, right) ->
      "(" ^ pretty_expr left ^ " " ^ pretty_binop op ^ " " ^ pretty_expr right
      ^ ")"
  | If (_, condition, then_branch, else_branch) ->
      "(if " ^ pretty_expr condition ^ " then {\n" ^ "    "
      ^ pretty_expr then_branch ^ "\n" ^ "} else {\n" ^ "    "
      ^ pretty_expr else_branch ^ "\n" ^ "})"
  | Sequence statements ->
      "{\n"
      ^ String.concat ";\n"
          (List.map (fun x -> "  " ^ pretty_statement x) statements)
      ^ "\n}"

and pretty_statement = function
  | Let (_, name, body) -> "let " ^ name ^ " = " ^ pretty_expr body
  | LetFun(_, name, params, body) -> "let " ^ name ^ "(" ^ String.concat ", " params ^ ") = " ^ pretty_expr body
  | RunExpr expr -> pretty_expr expr

let rec pretty_value = function
  | Number f -> string_of_float f
  | String str -> "\"" ^ str ^ "\""
  | Closure (_, params, expr) ->
      "(\\[closure](" ^ String.concat ", " params ^ ") -> " ^ pretty_expr expr
      ^ ")"
  | Bool bool -> string_of_bool bool
  | List list -> "[" ^ String.concat ", " (List.map pretty_value list) ^ "]"
  | Nil -> "nil"

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
