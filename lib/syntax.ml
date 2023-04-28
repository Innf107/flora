type name = string

module NameMap = Map.Make (String)

type loc = Loc.t

type literal =
  | NumberLit of float
  | StringLit of string
  | NilLit

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
  | Number of float
  | String of string
  | Bool of bool
  | List of value list
  | Closure of env * name list * expr
  | Nil

and env = { variables : value NameMap.t }

let pretty_literal = function
  | NumberLit f -> string_of_float f
  | StringLit str -> "\"" ^ str ^ "\""
  | NilLit -> "nil"

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
    "(if " ^ pretty_expr condition ^ " then {\n" 
      ^ "    " ^ pretty_expr then_branch ^ "\n"
      ^ "} else {\n"
      ^ "    " ^ pretty_expr else_branch ^ "\n"
      ^ "})"
  | Sequence(statements) ->
    "{\n"
    ^ String.concat ";\n" (List.map (fun x -> "  " ^ pretty_statement x) statements)
    ^ "\n}"

and pretty_statement = function
  | Let (_, name, body) ->
    "let " ^ name ^ " = " ^ pretty_expr body
  | RunExpr(expr) -> pretty_expr expr

let rec pretty_value = function
  | Number f -> string_of_float f
  | String str -> "\"" ^ str ^ "\""
  | Closure (_, params, expr) ->
      "(\\[closure](" ^ String.concat ", " params ^ ") -> " ^ pretty_expr expr
      ^ ")"
  | Bool bool -> string_of_bool bool
  | List list -> "[" ^ String.concat ", " (List.map pretty_value list) ^ "]"
  | Nil -> "nil"
