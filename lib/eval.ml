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

let bind_variable name value env =
  { env with variables = NameMap.add name value env.variables }

let eval_literal = function
  | NumberLit x -> Number x
  | StringLit str -> String str

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
      match NameMap.find_opt name env.variables with
      | None -> raise (EvalError (loc, VarNotFound name))
      | Some value -> (env, value)
    end
  | App (loc, function_expr, argument_exprs) -> begin
      match eval env function_expr with
      | _, Closure (closure_env, closure_names, body) ->
          let arguments =
            List.map (fun arg -> snd (eval env arg)) argument_exprs
          in

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
              List.fold_right2 bind_variable closure_names arguments closure_env
            in
            eval updated_closure_env body
          end
      | _, value -> raise (EvalError (loc, TryingToCallNonFunction value))
    end
  | Lambda (loc, names, body) -> (env, Closure (env, names, body))
  | Let (loc, name, body, rest) ->
      let _, value = eval env body in
      eval (bind_variable name value env) rest
  | Literal (loc, literal) -> (env, eval_literal literal)
  | Binop (loc, left, op, right) -> (env, eval_binop env loc left op right)
  | If (loc, condition, then_branch, else_branch) ->
      let _, condition_value = eval env condition in
      begin
        match condition_value with
        | Bool true -> eval env then_branch
        | Bool false -> eval env else_branch
        | _ -> invalid_operator_args loc "if" [ "Bool" ] [ condition_value ]
      end

and eval_binop env loc left op right =
  match op with
  | #strict_binop as op ->
      let _, left_value = eval env left in
      let _, right_value = eval env right in
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
          | _, Bool true -> Bool true
          | _, Bool false ->
              let _, result = eval env right in
              result
          | _, value ->
              invalid_operator_args loc "(||)" [ "Bool"; "_" ] [ value ]
        end
      | `And -> begin match eval env left with _, _ -> todo __LOC__ end
    end
