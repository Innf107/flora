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

let invalid_operator_args loc operator expected actual =
  raise
    (EvalError
       ( loc,
         InvalidOperatorArgs
           { operator; expected; actual = List.map (fun x -> Some x) actual } ))

let eval_literal = function
  | NumberLit x -> Number x
  | StringLit str -> String str
  | NilLit -> Nil
  | BoolLit bool -> Bool bool

let rec equal_value left_value right_value =
  match (left_value, right_value) with
  | Nil, Nil -> true
  | Number x, Number y -> Float.equal x y
  | String x, String y -> String.equal x y
  | Bool x, Bool y -> Bool.equal x y
  | List xs, List ys ->
      List.compare_lengths xs ys = 0 && List.for_all2 equal_value xs ys
  | Record fields1, Record fields2 ->
      RecordMap.equal equal_value fields1 fields2
  | _ -> false

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

let rec match_pattern : pattern -> value -> (env -> env) option =
  let ( let* ) = Option.bind in
  function
  | LiteralPat (loc, NilLit) -> begin
      function
      | Nil -> Some Fun.id
      | _ -> None
    end
  | LiteralPat (loc, NumberLit float) -> begin
      function
      | Number float2 when Float.equal float float2 -> Some Fun.id
      | _ -> None
    end
  | LiteralPat (loc, StringLit str) -> begin
      function
      | String str2 when String.equal str str2 -> Some Fun.id
      | _ -> None
    end
  | LiteralPat (loc, BoolLit bool) -> begin
      function
      | Bool bool2 when Bool.equal bool bool2 -> Some Fun.id
      | _ -> None
    end
  | VarPat (loc, name) -> fun value -> Some (bind_variable name value)
  | ListPat (loc, patterns) -> begin
      function
      | List values when List.compare_lengths patterns values = 0 ->
          Option.map Util.compose
            (Util.traverse_option
               (fun (pattern, value) -> match_pattern pattern value)
               (List.combine patterns values))
      | _ -> None
    end
  | ConsPat (loc, head_pat, tail_pat) -> begin
      function
      | List (head :: tail) ->
          let* head_trans = match_pattern head_pat head in
          let* tail_trans = match_pattern tail_pat (List tail) in
          Some (fun env -> tail_trans (head_trans env))
      | _ -> None
    end
  | RecordPat (loc, entries) -> begin
      function
      | Record record ->
          let find_entry (name, pattern) =
            let* value = RecordMap.find_opt name record in
            match_pattern pattern value
          in
          Option.map Util.compose (Util.traverse_option find_entry entries)
      | _ -> None
    end
  | OrPat (loc, left, right) ->
      fun value ->
        begin
          match match_pattern left value with
          | Some env_trans -> Some env_trans
          | None -> match_pattern right value
        end
  | AsPat (loc, pattern, name) ->
      fun value ->
        begin
          match match_pattern pattern value with
          | None -> None
          | Some env_trans ->
              Some (fun env -> bind_variable name value (env_trans env))
        end

let match_pattern_exn : pattern -> value -> env -> env =
 fun pattern value ->
  match match_pattern pattern value with
  | Some trans -> trans
  | None ->
      raise
        (EvalError
           (pattern_loc pattern, NonexhaustivePatterns { scrutinee = value }))

let match_patterns_exn : (pattern * value) Seq.t -> env -> env =
 fun patterns ->
  let transformers =
    Seq.map (fun (pattern, value) -> match_pattern_exn pattern value) patterns
  in
  Util.compose_seq transformers

let rec continue : type a r. (a, r) cont -> a -> r eval_result =
 fun cont argument ->
  match cont with
  | Done -> Completed argument
  | EvalAppFun (loc, env, argument_exprs, cont) -> begin
      match argument with
      | Closure (closure_env, closure_names, body) ->
          if List.compare_lengths closure_names argument_exprs <> 0 then
            raise
              (EvalError
                 ( loc,
                   IncorrectNumberOfArgs
                     {
                       expected = List.length closure_names;
                       actual = List.length argument_exprs;
                     } ))
          else begin
            match argument_exprs with
            | [] -> eval_cont env body cont
            | arg :: rest ->
                eval_cont env arg
                  (EvalAppArgs
                     ( (Lazy.force_val closure_env, closure_names, body),
                       env,
                       [],
                       rest,
                       cont ))
          end
      | Primop primop -> begin
          match argument_exprs with
          | [] -> eval_primop env loc primop [] cont
          | expr :: rest ->
              eval_cont env expr
                (EvalAppPrimop (primop, env, loc, [], rest, cont))
        end
      | Continuation called_continuation -> begin
          match argument_exprs with
          | [ arg_expr ] ->
              let called_continuation : (value, value) cont =
                Obj.magic called_continuation
              in
              eval_cont env arg_expr (Compose (called_continuation, cont))
          | args ->
              raise
                (EvalError
                   (loc, IncorrectNumberOfArgsToContinuation (List.length args)))
        end
      | value -> raise (EvalError (loc, TryingToCallNonFunction value))
    end
  | EvalAppArgs
      ( ((closure_env, closure_names, closure_body) as closure),
        env,
        arg_values,
        exprs,
        cont ) -> begin
      match exprs with
      | [] ->
          let updated_env =
            match_patterns_exn
              (Seq.zip
                 (List.to_seq closure_names)
                 (List.to_seq (List.rev (argument :: arg_values))))
              closure_env
          in
          eval_cont updated_env closure_body cont
      | expr :: rest ->
          eval_cont env expr
            (EvalAppArgs (closure, env, argument :: arg_values, rest, cont))
    end
  | EvalAppPrimop (primop, env, loc, arg_values, arg_exprs, cont) -> begin
      match arg_exprs with
      | [] ->
          eval_primop env loc primop (List.rev (argument :: arg_values)) cont
      | expr :: rest ->
          eval_cont env expr
            (EvalAppPrimop (primop, env, loc, argument :: arg_values, rest, cont))
    end
  | IfCont (loc, env, then_branch, else_branch, cont) -> begin
      match argument with
      | Bool true -> eval_cont env then_branch cont
      | Bool false -> eval_cont env else_branch cont
      | condition_value ->
          invalid_operator_args loc "if" [ "Bool" ] [ condition_value ]
    end
  | WithEnv (env, cont) -> continue cont (env, argument)
  | IgnoreEnv cont ->
      let env, value = argument in
      continue cont value
  | EvalSequence (env, statements, cont) -> eval_statements env statements cont
  | BindValue (env, pattern, statements, cont) ->
      let updated_env = match_pattern_exn pattern argument env in
      eval_statements updated_env statements cont
  | StrictBinOp1 (loc, env, op, right, cont) ->
      eval_cont env right (StrictBinOp2 (loc, argument, op, cont))
  | StrictBinOp2 (loc, left, op, cont) ->
      (* No idea why this is needed but if we remove this line it doesn't compile*)
      let argument : value = argument in
      continue cont (eval_strict_binop loc left op argument)
  | LazyBinOp (loc, env, op, right, cont) -> (
      match op with
      | `Or -> begin
          match argument with
          | Bool true -> continue cont (Bool true)
          | Bool false -> eval_cont env right cont
          | value -> invalid_operator_args loc "(||)" [ "Bool"; "_" ] [ value ]
        end
      | `And -> begin
          match argument with
          | Bool true -> eval_cont env right cont
          | Bool false -> continue cont (Bool false)
          | value -> invalid_operator_args loc "(&&)" [ "Bool"; "_" ] [ value ]
        end)
  | PerformArgs (loc, env, effect, arguments, argument_values, cont) -> begin
      match arguments with
      | [] -> Suspended (effect, List.rev (argument :: argument_values), cont)
      | expr :: rest ->
          eval_cont env expr
            (PerformArgs
               (loc, env, effect, rest, argument :: argument_values, cont))
    end
  | Compose (ab_cont, bc_cont) -> begin
      match continue ab_cont argument with
      | Completed result -> continue bc_cont result
      | Suspended (effect, arguments, partial_cont) ->
          Suspended (effect, arguments, Compose (partial_cont, bc_cont))
    end
  | EvalListLiteral (env, values, exprs, cont) -> begin
      match exprs with
      | [] -> continue cont (List (List.rev (argument :: values)))
      | expr :: rest ->
          eval_cont env expr
            (EvalListLiteral (env, argument :: values, rest, cont))
    end
  | EvalRecordLiteral (env, name, built_map, bindings, cont) -> begin
      let built_map = RecordMap.add name argument built_map in
      match bindings with
      | [] -> continue cont (Record built_map)
      | (name, expr) :: bindings ->
          eval_cont env expr
            (EvalRecordLiteral (env, name, built_map, bindings, cont))
    end
  | EvalMatch (loc, env, branches, cont) ->
      let rec go branches =
        match branches with
        | [] ->
            raise
              (EvalError (loc, NonexhaustivePatterns { scrutinee = argument }))
        | (pattern, body) :: branches -> begin
            match match_pattern pattern argument with
            | None -> go branches
            | Some env_trans -> eval_cont (env_trans env) body cont
          end
      in
      go branches

and eval_cont : type r. env -> expr -> (value, r) cont -> r eval_result =
 fun env expr cont ->
  match expr with
  | Var (loc, name) -> begin
      match NameMap.find_opt name env.contents.variables with
      | None -> begin
          match parse_primop name with
          | None -> raise (EvalError (loc, VarNotFound name))
          | Some primop -> continue cont (Primop primop)
        end
      | Some value -> continue cont value
    end
  | App (loc, function_expr, argument_exprs) ->
      eval_cont env function_expr (EvalAppFun (loc, env, argument_exprs, cont))
  | Lambda (loc, names, body) -> continue cont (Closure (lazy env, names, body))
  | Literal (loc, literal) -> continue cont (eval_literal literal)
  | ListLiteral (loc, elements) -> begin
      match elements with
      | [] -> continue cont (List [])
      | expr :: exprs ->
          eval_cont env expr (EvalListLiteral (env, [], exprs, cont))
    end
  | RecordLiteral (loc, bindings) -> begin
      match bindings with
      | [] -> continue cont (Record RecordMap.empty)
      | (name, expr) :: bindings ->
          eval_cont env expr
            (EvalRecordLiteral (env, name, RecordMap.empty, bindings, cont))
    end
  | Binop (loc, left, op, right) -> eval_binop env loc left op right cont
  | If (loc, condition, then_branch, else_branch) ->
      eval_cont env condition
        (IfCont (loc, env, then_branch, else_branch, cont))
  | Sequence statements -> eval_statements env statements (IgnoreEnv cont)
  | Perform (loc, effect, arguments) -> begin
      match arguments with
      | [] -> Suspended (effect, [], cont)
      | expr :: rest ->
          eval_cont env expr (PerformArgs (loc, env, effect, rest, [], cont))
    end
  | Handle (loc, scrutinee, handlers) -> begin
      match eval_cont env scrutinee Done with
      | Completed value -> continue cont value
      | Suspended (effect, arguments, suspended_cont) -> begin
          match
            List.find_opt
              (fun (handled_effect, _, _, _) ->
                (* I don't like that we need to use string equality here.
                   We should probably intern strings. *)
                String.equal effect handled_effect)
              handlers
          with
          | Some (_, parameter_names, cont_name, body) ->
              if List.compare_lengths parameter_names arguments <> 0 then begin
                raise
                  (EvalError
                     ( loc,
                       IncorrectNumberOfHandlerArgs
                         {
                           effect;
                           expected = List.length parameter_names;
                           actual = List.length arguments;
                         } ))
              end
              else begin
                let argument_seq =
                  Seq.zip (List.to_seq parameter_names) (List.to_seq arguments)
                in

                let updated_env =
                  match_patterns_exn
                    (Seq.cons
                       ( VarPat (loc, cont_name),
                         Continuation (Obj.magic suspended_cont) )
                       argument_seq)
                    env
                in

                eval_cont updated_env body cont
              end
          | None -> Suspended (effect, arguments, Compose (suspended_cont, cont))
        end
    end
  | Match (loc, scrutinee, branches) ->
      eval_cont env scrutinee (EvalMatch (loc, env, branches, cont))

and eval_primop :
    type r.
    env -> loc -> primop -> value list -> (value, r) cont -> r eval_result =
 fun env loc primop arguments cont ->
  match primop with
  | DynamicVar -> begin
      match arguments with
      | [ String var_name ] -> (
          match NameMap.find_opt var_name env.contents.variables with
          | Some value -> continue cont value
          | None -> continue cont Nil)
      | values ->
          raise
            (EvalError
               ( loc,
                 PrimopArgumentError
                   { primop; expected = "(String)"; actual = values } ))
    end

and eval_statements :
    type r. env -> statement list -> (env * value, r) cont -> r eval_result =
 fun env statements cont ->
  match statements with
  | [] -> continue cont (env, Nil)
  | [ RunExpr expr ] -> eval_cont env expr (WithEnv (env, cont))
  | RunExpr expr :: rest -> eval_cont env expr (EvalSequence (env, rest, cont))
  | Let (loc, name, body) :: rest ->
      eval_cont env body (BindValue (env, name, rest, cont))
  | LetFun (loc, name, params, body) :: rest ->
      let rec new_env =
        lazy (bind_variable name (Closure (new_env, params, body)) env)
      in
      eval_statements (Lazy.force new_env) rest cont

and eval_binop :
    type r.
    env -> loc -> expr -> binop -> expr -> (value, r) cont -> r eval_result =
 fun env loc left op right cont ->
  match op with
  | #strict_binop as op ->
      eval_cont env left (StrictBinOp1 (loc, env, op, right, cont))
  | #lazy_binop as op -> begin
      eval_cont env left (LazyBinOp (loc, env, op, right, cont))
    end

and eval_strict_binop loc left_value (op : strict_binop) right_value =
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

let eval env expr = eval_cont env expr Done
let eval_statements env statements = eval_statements env statements Done
