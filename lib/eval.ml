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
  | BoolLit bool -> Bool bool

let rec equal_value left_value right_value =
  match (left_value, right_value) with
  | Number x, Number y -> Float.equal x y
  | String x, String y -> String.equal x y
  | Bool x, Bool y -> Bool.equal x y
  | List xs, List ys ->
      List.compare_lengths xs ys = 0 && List.for_all2 equal_value xs ys
  | _ -> false

let ( let* ) = Util.bind_cps

let rec eval_cont : type r. env -> expr -> (value -> r) -> r =
 fun env expr cont ->
  match expr with
  | Var (loc, name) -> begin
      match NameMap.find_opt name env.contents.variables with
      | None -> raise (EvalError (loc, VarNotFound name))
      | Some value -> cont value
    end
  | App (loc, function_expr, argument_exprs) ->
      let* fun_value = eval_cont env function_expr in
      begin
        match fun_value with
        | Closure (closure_env, closure_names, body) ->
            let* arguments = Util.map_cps (eval_cont env) argument_exprs in
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
              eval_cont updated_closure_env body cont
            end
        | value -> raise (EvalError (loc, TryingToCallNonFunction value))
      end
  | Lambda (loc, names, body) -> cont (Closure (env, names, body))
  | Literal (loc, literal) -> cont (eval_literal literal)
  | Binop (loc, left, op, right) -> eval_binop env loc left op right cont
  | If (loc, condition, then_branch, else_branch) ->
      let* condition_value = eval_cont env condition in
      begin
        match condition_value with
        | Bool true -> eval_cont env then_branch cont
        | Bool false -> eval_cont env else_branch cont
        | condition_value ->
            invalid_operator_args loc "if" [ "Bool" ] [ condition_value ]
      end
  | Sequence statements ->
      eval_statements env statements (fun _env value -> cont value)

and eval_statements : 'r. env -> statement list -> (env -> value -> 'r) -> 'r =
 fun env statements cont ->
  match statements with
  | [] -> cont env Nil
  | [ RunExpr expr ] -> eval_cont env expr (cont env)
  | RunExpr expr :: rest ->
      let* _ = eval_cont env expr in
      eval_statements env rest cont
  | Let (loc, name, body) :: rest ->
      let* value = eval_cont env body in
      eval_statements (bind_variable name value env) rest cont

and eval_binop :
    type r. env -> loc -> expr -> binop -> expr -> (value -> r) -> r =
 fun env loc left op right cont ->
  match op with
  | #strict_binop as op ->
      let* left_value = eval_cont env left in

      let* right_value = eval_cont env right in
      begin
        match op with
        | `Add -> begin
            match (left_value, right_value) with
            | Number x, Number y -> cont (Number (x +. y))
            | _ ->
                invalid_operator_args loc "(+)" [ "Number"; "Number" ]
                  [ left_value; right_value ]
          end
        | `Subtract -> begin
            match (left_value, right_value) with
            | Number x, Number y -> cont (Number (x -. y))
            | _ ->
                invalid_operator_args loc "(-)" [ "Number"; "Number" ]
                  [ left_value; right_value ]
          end
        | `Multiply -> begin
            match (left_value, right_value) with
            | Number x, Number y -> cont (Number (x *. y))
            | _ ->
                invalid_operator_args loc "(*)" [ "Number"; "Number" ]
                  [ left_value; right_value ]
          end
        | `Divide -> begin
            match (left_value, right_value) with
            | Number x, Number y -> cont (Number (x /. y))
            | _ ->
                invalid_operator_args loc "(/)" [ "Number"; "Number" ]
                  [ left_value; right_value ]
          end
        | `Less -> begin
            match (left_value, right_value) with
            | Number x, Number y -> cont (Bool (x < y))
            | _ ->
                invalid_operator_args loc "(<)" [ "Number"; "Number" ]
                  [ left_value; right_value ]
          end
        | `LessOrEqual -> begin
            match (left_value, right_value) with
            | Number x, Number y -> cont (Bool (x <= y))
            | _ ->
                invalid_operator_args loc "(<=)" [ "Number"; "Number" ]
                  [ left_value; right_value ]
          end
        | `Equal -> cont (Bool (equal_value left_value right_value))
        | `NotEqual -> cont (Bool (not (equal_value left_value right_value)))
        | `GreaterOrEqual -> begin
            match (left_value, right_value) with
            | Number x, Number y -> cont (Bool (x >= y))
            | _ ->
                invalid_operator_args loc "(>=)" [ "Number"; "Number" ]
                  [ left_value; right_value ]
          end
        | `Greater -> begin
            match (left_value, right_value) with
            | Number x, Number y -> cont (Bool (x > y))
            | _ ->
                invalid_operator_args loc "(>)" [ "Number"; "Number" ]
                  [ left_value; right_value ]
          end
        | `Cons -> begin
            match right_value with
            | List values -> cont (List (left_value :: values))
            | _ ->
                invalid_operator_args loc "(:)" [ "_"; "List(_)" ]
                  [ left_value; right_value ]
          end
        | `Concat -> begin
            match (left_value, right_value) with
            | String x, String y -> cont (String (x ^ y))
            | List left, List right -> cont (List (left @ right))
            | _ ->
                invalid_operator_args loc "(~)" [ "List(_)"; "List(_)" ]
                  [ left_value; right_value ]
          end
      end
  | #lazy_binop as op -> begin
      match op with
      | `Or -> begin
          let* left_value = eval_cont env left in
          match left_value with
          | Bool true -> cont (Bool true)
          | Bool false -> eval_cont env right cont
          | value -> invalid_operator_args loc "(||)" [ "Bool"; "_" ] [ value ]
        end
      | `And -> begin
          let* left_value = eval_cont env left in
          match left_value with
          | Bool true -> eval_cont env right cont
          | Bool false -> cont (Bool false)
          | value -> invalid_operator_args loc "(&&)" [ "Bool"; "_" ] [ value ]
        end
    end

let eval env expr = eval_cont env expr Fun.id

let eval_statements env statements =
  eval_statements env statements (fun env value -> (env, value))
