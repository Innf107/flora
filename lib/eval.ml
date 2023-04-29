open Syntax
open Util

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

exception EvalError of loc * eval_error

let invalid_operator_args loc operator expected actual =
  raise
    (EvalError
       ( loc,
         InvalidOperatorArgs
           { operator; expected; actual = List.map (fun x -> Some x) actual } ))

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

let eval_literal = function
  | NumberLit x -> Number x
  | StringLit str -> String str
  | NilLit -> Nil

let rec equal_value left_value right_value =
  match (left_value, right_value) with
  | Number x, Number y -> Float.equal x y
  | String x, String y -> String.equal x y
  | Bool x, Bool y -> Bool.equal x y
  | List xs, List ys ->
      List.compare_lengths xs ys = 0 && List.for_all2 equal_value xs ys
  | _ -> false

(* TODO: This is not tail recursive, so memory usage will be linear in program execution.
   It shouldn't overflow though thanks to OCaml 5 *)
let rec eval env = function
  | Var (loc, name) -> begin
      match NameMap.find_opt name env.contents.variables with
      | None -> raise (EvalError (loc, VarNotFound name))
      | Some value -> value
    end
  | App (loc, function_expr, argument_exprs) -> begin
      match eval env function_expr with
      | Closure (closure_env, closure_names, body) ->
          let arguments = List.map (eval env) argument_exprs in

          if List.compare_lengths closure_names arguments <> 0 then
            raise
              (EvalError
                 ( loc,
                   IncorrectNumberOfArgs
                     {
                       expected = List.length closure_names;
                       actual = List.length arguments;
                     } ))
          else begin
            let updated_closure_env =
              bind_variables
                (Seq.zip (List.to_seq closure_names) (List.to_seq arguments))
                closure_env
            in
            eval updated_closure_env body
          end
      | value -> raise (EvalError (loc, TryingToCallNonFunction value))
    end
  | Lambda (loc, names, body) -> Closure (env, names, body)
  | Literal (loc, literal) -> eval_literal literal
  | Binop (loc, left, op, right) -> eval_binop env loc left op right
  | If (loc, condition, then_branch, else_branch) -> begin
      match eval env condition with
      | Bool true -> eval env then_branch
      | Bool false -> eval env else_branch
      | condition_value ->
          invalid_operator_args loc "if" [ "Bool" ] [ condition_value ]
    end
  | Sequence statements ->
      let _env, value = eval_statements env statements in
      value

and eval_statements env = function
  | [] -> (env, Nil)
  | [ RunExpr expr ] -> (env, eval env expr)
  | RunExpr expr :: rest ->
      let _ = eval env expr in
      eval_statements env rest
  | Let (loc, name, body) :: rest ->
      let value = eval env body in
      eval_statements (bind_variable name value env) rest

and eval_binop env loc left op right =
  match op with
  | #strict_binop as op ->
      let left_value = eval env left in
      let right_value = eval env right in
      begin
        match op with
        | `Add -> begin
            match (left_value, right_value) with
            | Number x, Number y -> Number (x +. y)
            | _ ->
                invalid_operator_args loc "(+)" [ "Number"; "Number" ]
                  [ left_value; right_value ]
          end
        | `Subtract -> begin
            match (left_value, right_value) with
            | Number x, Number y -> Number (x -. y)
            | _ ->
                invalid_operator_args loc "(-)" [ "Number"; "Number" ]
                  [ left_value; right_value ]
          end
        | `Multiply -> begin
            match (left_value, right_value) with
            | Number x, Number y -> Number (x *. y)
            | _ ->
                invalid_operator_args loc "(*)" [ "Number"; "Number" ]
                  [ left_value; right_value ]
          end
        | `Divide -> begin
            match (left_value, right_value) with
            | Number x, Number y -> Number (x /. y)
            | _ ->
                invalid_operator_args loc "(/)" [ "Number"; "Number" ]
                  [ left_value; right_value ]
          end
        | `Less -> begin
            match (left_value, right_value) with
            | Number x, Number y -> Bool (x < y)
            | _ ->
                invalid_operator_args loc "(<)" [ "Number"; "Number" ]
                  [ left_value; right_value ]
          end
        | `LessOrEqual -> begin
            match (left_value, right_value) with
            | Number x, Number y -> Bool (x <= y)
            | _ ->
                invalid_operator_args loc "(<=)" [ "Number"; "Number" ]
                  [ left_value; right_value ]
          end
        | `Equal -> Bool (equal_value left_value right_value)
        | `NotEqual -> Bool (not (equal_value left_value right_value))
        | `GreaterOrEqual -> begin
            match (left_value, right_value) with
            | Number x, Number y -> Bool (x >= y)
            | _ ->
                invalid_operator_args loc "(>=)" [ "Number"; "Number" ]
                  [ left_value; right_value ]
          end
        | `Greater -> begin
            match (left_value, right_value) with
            | Number x, Number y -> Bool (x > y)
            | _ ->
                invalid_operator_args loc "(>)" [ "Number"; "Number" ]
                  [ left_value; right_value ]
          end
        | `Cons -> begin
            match right_value with
            | List values -> List (left_value :: values)
            | _ ->
                invalid_operator_args loc "(:)" [ "_"; "List(_)" ]
                  [ left_value; right_value ]
          end
        | `Concat -> begin
            match (left_value, right_value) with
            | String x, String y -> String (x ^ y)
            | List left, List right -> List (left @ right)
            | _ ->
                invalid_operator_args loc "(~)" [ "List(_)"; "List(_)" ]
                  [ left_value; right_value ]
          end
      end
  | #lazy_binop as op -> begin
      match op with
      | `Or -> begin
          match eval env left with
          | Bool true -> Bool true
          | Bool false -> eval env right
          | value -> invalid_operator_args loc "(||)" [ "Bool"; "_" ] [ value ]
        end
      | `And -> begin
          match eval env left with
          | Bool true -> eval env right
          | Bool false -> Bool false
          | value -> invalid_operator_args loc "(&&)" [ "Bool"; "_" ] [ value ]
        end
    end
