type name = string

module NameMap = Map.Make (String)

type literal =
  | NumberLit of float
  | StringLit of string

type loc = Loc.t

type expr =
  | Var of loc * name
  | App of loc * expr * expr list
  | Lambda of loc * name list * expr
  | Let of loc * name * expr * expr
  | Literal of loc * literal

and value =
  | Number of float
  | String of string
  | Closure of env * name list * expr

and env = { variables : value NameMap.t }

let pretty_literal = function
  | NumberLit f -> string_of_float f
  | StringLit str -> "\"" ^ str ^ "\""

let rec pretty_expr = function
  | Var (_, name) -> name
  | App (_, fun_expr, args) ->
      pretty_expr fun_expr ^ "("
      ^ String.concat ", " (List.map pretty_expr args)
      ^ ")"
  | Lambda (_, parameters, body) ->
      "(\\" ^ String.concat ", " parameters ^ " -> " ^ pretty_expr body ^ ")"
  | Let (_, name, body, rest) ->
      "let " ^ name ^ " = " ^ pretty_expr body ^ ";\n" ^ pretty_expr rest
  | Literal (_, literal) -> pretty_literal literal

let rec pretty_value = function
  | Number f -> string_of_float f
  | String str -> "\"" ^ str ^ "\""
  | Closure (_, params, expr) ->
      "(\\[closure](" ^ String.concat ", " params ^ ") -> " ^ pretty_expr expr
      ^ ")"
