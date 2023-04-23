open Syntax
open Util

type eval_error =
  | VarNotFound of string
  | TryingToCallNonFunction of value
  | IncorrectNumberOfArgs of {
      expected : int;
      actual : int;
    }

exception EvalError of loc * eval_error

let bind_variable name value env =
  { env with variables = NameMap.add name value env.variables }

let eval_literal = function
  | NumberLit x -> Number x
  | StringLit str -> String str

(* TODO: This is not tail recursive, so memory usage will be linear in program execution.
   It shouldn't segfault though thanks to OCaml 5 *)
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
