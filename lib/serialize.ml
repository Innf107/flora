open Syntax

let deserialize_category, trace_deserialize =
  Trace.make ~flag:"deserialize" ~prefix:"DESERIALIZE"

type deserialization_error =
  | EOF
  | InvalidTag of {
      ty : string;
      tag : int;
    }
  | ContTypeError of {
      expected : string;
      actual : string;
    }
  | NotAFloraEnvironment

exception DeserializationError of deserialization_error

type write_state = {
  (* TODO: Would be nice to use a map here *)
  mutable environments : (int * env_contents) list;
  mutable env_count : int;
  environments_to_be_written : (int * int option * env_contents) Queue.t;
  out_channel : out_channel;
}

type read_state = { in_channel : in_channel }
type read_env = int

type read_value =
  (* We might not have read the entire environment yet so this needs to be 'read_env' *)
  | Closure of read_env * pattern list * expr
  (* The rest is just boilerplate copied from the definition of Syntax.value *)
  | Nil
  | Number of float
  | String of string
  | Bool of bool
  | List of read_value list
  | Primop of primop
  (* TODO: Continuations should be shared similar to environments *)
  | Continuation of read_cont
  | Record of read_value RecordMap.t

and read_cont =
  | Done : read_cont
  | EvalAppFun : loc * read_env * expr list * read_cont -> read_cont
  | EvalAppArgs :
      (read_env * pattern list * expr)
      * read_env
      * read_value list
      * expr list
      * read_cont
      -> read_cont
  | EvalAppPrimop :
      primop * read_env * loc * read_value list * expr list * read_cont
      -> read_cont
  | IfCont : loc * read_env * expr * expr * read_cont -> read_cont
  | WithEnv : read_env * read_cont -> read_cont
  | IgnoreEnv : read_cont -> read_cont
  | EvalSequence : read_env * statement list * read_cont -> read_cont
  | BindValue : read_env * pattern * statement list * read_cont -> read_cont
  | StrictBinOp1 : loc * read_env * strict_binop * expr * read_cont -> read_cont
  | StrictBinOp2 : loc * read_value * strict_binop * read_cont -> read_cont
  | LazyBinOp : loc * read_env * lazy_binop * expr * read_cont -> read_cont
  | PerformArgs :
      loc * read_env * name * expr list * read_value list * read_cont
      -> read_cont
  | Compose : read_cont * read_cont -> read_cont
  | EvalListLiteral :
      read_env * read_value list * expr list * read_cont
      -> read_cont
  | EvalRecordLiteral :
      read_env * name * read_value RecordMap.t * (name * expr) list * read_cont
      -> read_cont
  | EvalMatch : loc * read_env * (pattern * expr) list * read_cont -> read_cont

type read_env_contents = { variables : read_value NameMap.t }

let write_byte state byte = Out_channel.output_byte state.out_channel byte

let read_byte state =
  match In_channel.input_byte state.in_channel with
  | None -> raise (DeserializationError EOF)
  | Some byte -> byte

let write_optional :
    (write_state -> 'a -> unit) -> write_state -> 'a option -> unit =
 fun write_inner state -> function
  | None -> write_byte state 0
  | Some inner ->
      write_byte state 1;
      write_inner state inner

let read_optional : (read_state -> 'a) -> read_state -> 'a option =
 fun read_inner state ->
  match read_byte state with
  | 0 -> None
  | 1 ->
      let inner = read_inner state in
      Some inner
  | tag -> raise (DeserializationError (InvalidTag { ty = "optional"; tag }))

let write_int state int =
  (* Integers < 128 are represented as a single byte where the least significant bit is 0.
     This might seem like we loose a single bit of precision for 64 bit integers, but
     because OCaml does pointer tagging as well, we actually don't!
     `int`s are only 63 bit anyway, so we can store the remaining bit to store multibyte size information.
     Also, the fact that integers are stored in little endian byte order is crucial, since this
     ensures that the first byte we read contains the least significant bit *)
  if int >= 0 && int < 128 then
    (* int < 128 so the left shift will not destroy information *)
    Out_channel.output_byte state.out_channel (int lsl 1)
  else begin
    let int64 = Int64.logor (Int64.shift_left (Int64.of_int int) 1) 1L in
    let buffer = Bytes.create 8 in
    Bytes.set_int64_le buffer 0 int64;
    Out_channel.output_bytes state.out_channel buffer
  end

let read_int state =
  match In_channel.input_byte state.in_channel with
  | None -> raise (DeserializationError EOF)
  | Some byte when byte land 1 = 0 ->
      (* int < 128 *)
      byte lsr 1
  | Some initial_byte ->
      let buffer = Bytes.create 8 in
      Bytes.set buffer 0 (Char.chr initial_byte);
      begin
        match In_channel.really_input state.in_channel buffer 1 7 with
        | Some () ->
            Int64.to_int
              (Int64.shift_right_logical (Bytes.get_int64_le buffer 0) 1)
        | None -> raise (DeserializationError EOF)
      end

let write_float state float =
  let buffer = Bytes.create 8 in
  Bytes.set_int64_le buffer 0 (Int64.bits_of_float float);
  Out_channel.output_bytes state.out_channel buffer

let read_float state =
  let buffer = Bytes.create 8 in
  match In_channel.really_input state.in_channel buffer 0 8 with
  | Some () -> Int64.float_of_bits (Bytes.get_int64_le buffer 0)
  | None -> raise (DeserializationError EOF)

let write_bool state bool = write_byte state (if bool then 1 else 0)

let read_bool state =
  match read_byte state with
  | 0 -> false
  | 1 -> true
  | tag -> raise (DeserializationError (InvalidTag { ty = "bool"; tag }))

let write_list write_inner state list =
  write_int state (List.length list);
  List.iter (write_inner state) list

let read_list read_inner state =
  let size = read_int state in
  List.init size (fun _ -> read_inner state)

let write_string state str =
  write_int state (String.length str);
  Out_channel.output_string state.out_channel str

let read_string state =
  let size = read_int state in
  let bytes = Bytes.create size in
  match In_channel.really_input state.in_channel bytes 0 size with
  | Some () -> String.of_bytes bytes
  | None -> raise (DeserializationError EOF)

let rec register_env_index state env =
  match
    List.find_opt (fun (_, other) -> other == env.delta) state.environments
  with
  | Some (index, _) -> index
  | None ->
      let previous_index =
        match env.previous with
        | None -> None
        | Some env -> Some (register_env_index state env)
      in
      let index = state.env_count in
      state.environments <- (index, env.delta) :: state.environments;
      Queue.add
        (index, previous_index, env.delta)
        state.environments_to_be_written;
      state.env_count <- state.env_count + 1;
      index

let write_env state env =
  let index = register_env_index state env in
  write_int state index

let read_env = read_int

let write_loc state Loc.{ file; start_line; start_column; end_line; end_column }
    =
  (* TODO: Share the string to reduce file size drastically *)
  write_string state file;
  write_int state start_line;
  write_int state start_column;
  write_int state end_line;
  write_int state end_column

let read_loc state =
  let file = read_string state in
  let start_line = read_int state in
  let start_column = read_int state in
  let end_line = read_int state in
  let end_column = read_int state in
  Loc.{ file; start_line; start_column; end_line; end_column }

let write_literal state = function
  | Syntax.NilLit -> write_byte state 0
  | NumberLit float ->
      write_byte state 1;
      write_float state float
  | StringLit str ->
      write_byte state 2;
      write_string state str
  | BoolLit bool ->
      write_byte state 3;
      write_bool state bool

let read_literal state =
  match read_byte state with
  | 0 -> Syntax.NilLit
  | 1 ->
      let float = read_float state in
      NumberLit float
  | 2 ->
      let str = read_string state in
      StringLit str
  | 3 ->
      let bool = read_bool state in
      BoolLit bool
  | tag -> raise (DeserializationError (InvalidTag { ty = "literal"; tag }))

let write_binop state = function
  | (`Add : [< binop ]) -> write_byte state 0
  | `Subtract -> write_byte state 1
  | `Multiply -> write_byte state 2
  | `Divide -> write_byte state 3
  | `Less -> write_byte state 4
  | `LessOrEqual -> write_byte state 5
  | `Equal -> write_byte state 6
  | `NotEqual -> write_byte state 7
  | `GreaterOrEqual -> write_byte state 8
  | `Greater -> write_byte state 9
  | `Cons -> write_byte state 10
  | `Concat -> write_byte state 11
  | `Or -> write_byte state 12
  | `And -> write_byte state 13

let read_binop state : binop =
  match read_byte state with
  | 0 -> `Add
  | 1 -> `Subtract
  | 3 -> `Multiply
  | 4 -> `Divide
  | 5 -> `Less
  | 6 -> `LessOrEqual
  | 7 -> `Equal
  | 8 -> `NotEqual
  | 9 -> `Greater
  | 10 -> `Cons
  | 11 -> `Concat
  | 12 -> `Or
  | 13 -> `And
  | tag -> raise (DeserializationError (InvalidTag { ty = "binop"; tag }))

let rec write_pattern state = function
  | VarPat (loc, name) ->
      write_byte state 0;
      write_loc state loc;
      write_string state name
  | LiteralPat (loc, literal) ->
      write_byte state 1;
      write_loc state loc;
      write_literal state literal
  | ListPat (loc, patterns) ->
      write_byte state 2;
      write_loc state loc;
      write_list write_pattern state patterns
  | ConsPat (loc, head_pat, tail_pat) ->
      write_byte state 3;
      write_loc state loc;
      write_pattern state head_pat;
      write_pattern state tail_pat
  | RecordPat (loc, entries) ->
      write_byte state 4;
      write_loc state loc;
      write_list
        (fun state (name, pattern) ->
          write_string state name;
          write_pattern state pattern)
        state entries
  | OrPat (loc, left, right) ->
      write_byte state 5;
      write_loc state loc;
      write_pattern state left;
      write_pattern state right
  | AsPat (loc, pattern, name) ->
      write_byte state 6;
      write_loc state loc;
      write_pattern state pattern;
      write_string state name

let rec read_pattern state =
  match read_byte state with
  | 0 ->
      let loc = read_loc state in
      let name = read_string state in
      VarPat (loc, name)
  | 1 ->
      let loc = read_loc state in
      let literal = read_literal state in
      LiteralPat (loc, literal)
  | 2 ->
      let loc = read_loc state in
      let patterns = read_list read_pattern state in
      ListPat (loc, patterns)
  | 3 ->
      let loc = read_loc state in
      let head_pat = read_pattern state in
      let tail_pat = read_pattern state in
      ConsPat (loc, head_pat, tail_pat)
  | 4 ->
      let loc = read_loc state in
      let entries =
        read_list
          (fun state ->
            let name = read_string state in
            let pattern = read_pattern state in
            (name, pattern))
          state
      in
      RecordPat (loc, entries)
  | 5 ->
      let loc = read_loc state in
      let left = read_pattern state in
      let right = read_pattern state in
      OrPat (loc, left, right)
  | 6 ->
      let loc = read_loc state in
      let pattern = read_pattern state in
      let name = read_string state in
      AsPat (loc, pattern, name)
  | tag -> raise (DeserializationError (InvalidTag { ty = "pattern"; tag }))

let rec write_expr state = function
  | Syntax.Var (loc, name) ->
      write_byte state 0;
      write_loc state loc;
      write_string state name
  | App (loc, expr, args) ->
      write_byte state 1;
      write_loc state loc;
      write_expr state expr;
      write_list write_expr state args
  | Lambda (loc, params, body) ->
      write_byte state 2;
      write_loc state loc;
      write_list write_pattern state params;
      write_expr state body
  | Literal (loc, literal) ->
      write_byte state 3;
      write_loc state loc;
      write_literal state literal
  | Binop (loc, left, binop, right) ->
      write_byte state 4;
      write_loc state loc;
      write_expr state left;
      write_binop state binop;
      write_expr state right
  | If (loc, condition, then_branch, else_branch) ->
      write_byte state 5;
      write_loc state loc;
      write_expr state condition;
      write_expr state then_branch;
      write_expr state else_branch
  | Sequence statements ->
      write_byte state 6;
      write_list write_statement state statements
  | Perform (loc, effect_name, arguments) ->
      write_byte state 7;
      write_loc state loc;
      write_string state effect_name;
      write_list write_expr state arguments
  | Handle (loc, scrutinee, handlers) ->
      write_byte state 8;
      write_loc state loc;
      write_list
        (fun state (name, params, cont_name, expr) ->
          write_string state name;
          write_list write_pattern state params;
          write_string state cont_name;
          write_expr state expr)
        state handlers
  | ListLiteral (loc, elements) ->
      write_byte state 9;
      write_loc state loc;
      write_list write_expr state elements
  | RecordLiteral (loc, elements) ->
      write_byte state 10;
      write_loc state loc;
      write_list
        (fun state (name, expr) ->
          write_string state name;
          write_expr state expr)
        state elements
  | Match (loc, scrutinee, branches) ->
      write_byte state 11;
      write_loc state loc;
      write_expr state scrutinee;
      write_list
        (fun state (pattern, body) ->
          write_pattern state pattern;
          write_expr state body)
        state branches

and write_statement state = function
  | RunExpr expr ->
      write_byte state 0;
      write_expr state expr
  | Let (loc, name, expr) ->
      write_byte state 1;
      write_loc state loc;
      write_pattern state name;
      write_expr state expr
  | LetFun (loc, name, params, body) ->
      write_byte state 2;
      write_loc state loc;
      write_string state name;
      write_list write_pattern state params;
      write_expr state body

let rec read_expr state =
  match read_byte state with
  | 0 ->
      let loc = read_loc state in
      let name = read_string state in
      Var (loc, name)
  | 1 ->
      let loc = read_loc state in
      let expr = read_expr state in
      let args = read_list read_expr state in
      App (loc, expr, args)
  | 2 ->
      let loc = read_loc state in
      let params = read_list read_pattern state in
      let body = read_expr state in
      Lambda (loc, params, body)
  | 3 ->
      let loc = read_loc state in
      let literal = read_literal state in
      Literal (loc, literal)
  | 4 ->
      let loc = read_loc state in
      let left = read_expr state in
      let binop = read_binop state in
      let right = read_expr state in
      Binop (loc, left, binop, right)
  | 5 ->
      let loc = read_loc state in
      let condition = read_expr state in
      let then_branch = read_expr state in
      let else_branch = read_expr state in
      If (loc, condition, then_branch, else_branch)
  | 6 ->
      let statements = read_list read_statement state in
      Sequence statements
  | 7 ->
      let loc = read_loc state in
      let effect_name = read_string state in
      let arguments = read_list read_expr state in
      Perform (loc, effect_name, arguments)
  | 8 ->
      let loc = read_loc state in
      let scrutinee = read_expr state in
      let handlers =
        read_list
          (fun state ->
            let name = read_string state in
            let params = read_list read_pattern state in
            let cont_name = read_string state in
            let expr = read_expr state in
            (name, params, cont_name, expr))
          state
      in
      Handle (loc, scrutinee, handlers)
  | 9 ->
      let loc = read_loc state in
      let elements = read_list read_expr state in
      ListLiteral (loc, elements)
  | 10 ->
      let loc = read_loc state in
      let elements =
        read_list
          (fun state ->
            let name = read_string state in
            let expr = read_expr state in
            (name, expr))
          state
      in
      RecordLiteral (loc, elements)
  | 11 ->
      let loc = read_loc state in
      let scrutinee = read_expr state in
      let branches =
        read_list
          (fun state ->
            let pattern = read_pattern state in
            let expr = read_expr state in
            (pattern, expr))
          state
      in
      Match (loc, scrutinee, branches)
  | tag -> raise (DeserializationError (InvalidTag { ty = "expr"; tag }))

and read_statement state =
  match read_byte state with
  | 0 ->
      let expr = read_expr state in
      RunExpr expr
  | 1 ->
      let loc = read_loc state in
      let pattern = read_pattern state in
      let expr = read_expr state in
      Let (loc, pattern, expr)
  | 2 ->
      let loc = read_loc state in
      let name = read_string state in
      let params = read_list read_pattern state in
      let body = read_expr state in
      LetFun (loc, name, params, body)
  | tag -> raise (DeserializationError (InvalidTag { ty = "statement"; tag }))

let write_primop state = function
  | DynamicVar -> write_byte state 0

and read_primop state =
  match read_byte state with
  | 0 -> DynamicVar
  | tag -> raise (DeserializationError (InvalidTag { ty = "primop"; tag }))

let rec write_value state = function
  | Syntax.Nil -> write_int state 0
  | Number f ->
      write_byte state 1;
      write_float state f
  | String str ->
      write_byte state 2;
      write_string state str
  | Bool bool ->
      write_byte state 3;
      write_bool state bool
  | List values ->
      write_byte state 4;
      write_list write_value state values
  | Closure (env, parameters, body) ->
      write_byte state 5;
      write_env state (Lazy.force_val env);
      write_list write_pattern state parameters;
      write_expr state body
  | Primop primop ->
      write_byte state 6;
      write_primop state primop
  | Continuation cont ->
      write_byte state 7;
      let cont : (value, value) Eval.cont = Obj.magic cont in
      write_cont state cont
  | Record values ->
      write_byte state 8;
      write_list
        (fun state (name, value) ->
          write_string state name;
          write_value state value)
        state
        (List.of_seq (RecordMap.to_seq values))

and read_value state : read_value =
  let tag = read_byte state in
  match tag with
  | 0 -> Nil
  | 1 ->
      let f = read_float state in
      Number f
  | 2 ->
      let str = read_string state in
      String str
  | 3 ->
      let bool = read_bool state in
      Bool bool
  | 4 ->
      let values = read_list read_value state in
      List values
  | 5 ->
      let env = read_env state in
      let params = read_list read_pattern state in
      let body = read_expr state in
      Closure (env, params, body)
  | 6 ->
      let op = read_primop state in
      Primop op
  | 7 ->
      let cont = read_cont state in
      Continuation cont
  | 8 ->
      let values =
        read_list
          (fun state ->
            let name = read_string state in
            let value = read_value state in
            (name, value))
          state
      in
      Record (RecordMap.of_seq (List.to_seq values))
  | tag -> raise (DeserializationError (InvalidTag { ty = "value"; tag }))

and write_cont : type a b. write_state -> (a, b) Eval.cont -> unit =
 fun state -> function
  | Done -> write_byte state 0
  | EvalAppFun (loc, env, arguments, cont) ->
      write_byte state 1;
      write_loc state loc;
      write_env state env;
      write_list write_expr state arguments;
      write_cont state cont
  | EvalAppArgs
      ( (closure_env, closure_params, closure_body),
        env,
        arguments,
        argument_exprs,
        cont ) ->
      write_byte state 2;
      write_env state closure_env;
      write_list write_pattern state closure_params;
      write_expr state closure_body;
      write_env state env;
      write_list write_value state arguments;
      write_list write_expr state argument_exprs;
      write_cont state cont
  | EvalAppPrimop (primop, env, loc, arg_values, arg_exprs, cont) ->
      write_byte state 3;
      write_primop state primop;
      write_env state env;
      write_loc state loc;
      write_list write_value state arg_values;
      write_list write_expr state arg_exprs;
      write_cont state cont
  | IfCont (loc, env, then_branch, else_branch, cont) ->
      write_byte state 4;
      write_env state env;
      write_expr state then_branch;
      write_expr state else_branch;
      write_cont state cont
  | WithEnv (env, cont) ->
      write_byte state 5;
      write_env state env;
      write_cont state cont
  | IgnoreEnv cont ->
      write_byte state 6;
      write_cont state cont
  | EvalSequence (env, statements, cont) ->
      write_byte state 7;
      write_env state env;
      write_list write_statement state statements;
      write_cont state cont
  | BindValue (env, pattern, rest, cont) ->
      write_byte state 8;
      write_env state env;
      write_pattern state pattern;
      write_list write_statement state rest;
      write_cont state cont
  | StrictBinOp1 (loc, env, strict_binop, expr, cont) ->
      write_byte state 9;
      write_loc state loc;
      write_env state env;
      write_binop state strict_binop;
      write_expr state expr;
      write_cont state cont
  | StrictBinOp2 (loc, value, strict_binop, cont) ->
      write_byte state 10;
      write_loc state loc;
      write_value state value;
      write_binop state strict_binop;
      write_cont state cont
  | LazyBinOp (loc, env, lazy_binop, expr, cont) ->
      write_byte state 11;
      write_loc state loc;
      write_env state env;
      write_binop state lazy_binop;
      write_expr state expr;
      write_cont state cont
  | PerformArgs (loc, env, name, arg_exprs, arg_values, cont) ->
      write_byte state 12;
      write_loc state loc;
      write_env state env;
      write_string state name;
      write_list write_expr state arg_exprs;
      write_list write_value state arg_values;
      write_cont state cont
  | Compose (cont1, cont2) ->
      write_byte state 13;
      write_cont state cont1;
      write_cont state cont2
  | EvalListLiteral (env, values, exprs, cont) ->
      write_byte state 14;
      write_list write_value state values;
      write_list write_expr state exprs;
      write_cont state cont
  | EvalRecordLiteral (env, name, built_map, bindings, cont) ->
      write_byte state 15;
      write_env state env;
      write_string state name;
      write_list
        (fun state (name, value) ->
          write_string state name;
          write_value state value)
        state
        (List.of_seq (RecordMap.to_seq built_map));
      write_list
        (fun state (name, expr) ->
          write_string state name;
          write_expr state expr)
        state bindings;
      write_cont state cont
  | EvalMatch (loc, env, branches, cont) ->
      write_byte state 16;
      write_loc state loc;
      write_env state env;
      write_list
        (fun state (pattern, expr) ->
          write_pattern state pattern;
          write_expr state expr)
        state branches;
      write_cont state cont

and read_cont : read_state -> read_cont =
 fun state ->
  match read_byte state with
  | 0 -> Done
  | 1 ->
      let loc = read_loc state in
      let env = read_env state in
      let arguments = read_list read_expr state in
      let cont = read_cont state in
      EvalAppFun (loc, env, arguments, cont)
  | 2 ->
      let closure_env = read_env state in
      let closure_params = read_list read_pattern state in
      let closure_body = read_expr state in
      let env = read_env state in
      let arguments = read_list read_value state in
      let argument_exprs = read_list read_expr state in
      let cont = read_cont state in
      EvalAppArgs
        ( (closure_env, closure_params, closure_body),
          env,
          arguments,
          argument_exprs,
          cont )
  | 3 ->
      let primop = read_primop state in
      let env = read_env state in
      let loc = read_loc state in
      let arg_values = read_list read_value state in
      let arg_exprs = read_list read_expr state in
      let cont = read_cont state in
      EvalAppPrimop (primop, env, loc, arg_values, arg_exprs, cont)
  | 4 ->
      let loc = read_loc state in
      let env = read_env state in
      let then_branch = read_expr state in
      let else_branch = read_expr state in
      let cont = read_cont state in
      IfCont (loc, env, then_branch, else_branch, cont)
  | 5 ->
      let env = read_env state in
      let cont = read_cont state in
      WithEnv (env, cont)
  | 6 ->
      let cont = read_cont state in
      IgnoreEnv cont
  | 7 ->
      let env = read_env state in
      let statements = read_list read_statement state in
      let cont = read_cont state in
      EvalSequence (env, statements, cont)
  | 8 ->
      let env = read_env state in
      let pattern = read_pattern state in
      let rest = read_list read_statement state in
      let cont = read_cont state in
      BindValue (env, pattern, rest, cont)
  | 9 ->
      let loc = read_loc state in
      let env = read_env state in
      let strict_binop =
        match read_binop state with
        | #strict_binop as op -> op
        | _ ->
            raise
              (DeserializationError
                 (InvalidTag { ty = "strict_binop"; tag = -1 }))
      in
      let expr = read_expr state in
      let cont = read_cont state in
      StrictBinOp1 (loc, env, strict_binop, expr, cont)
  | 10 ->
      let loc = read_loc state in
      let value = read_value state in
      let strict_binop =
        match read_binop state with
        | #strict_binop as op -> op
        | _ ->
            raise
              (DeserializationError
                 (InvalidTag { ty = "strict_binop"; tag = -1 }))
      in
      let cont = read_cont state in
      StrictBinOp2 (loc, value, strict_binop, cont)
  | 11 ->
      let loc = read_loc state in
      let env = read_env state in
      let lazy_binop =
        match read_binop state with
        | #lazy_binop as op -> op
        | _ ->
            raise
              (DeserializationError (InvalidTag { ty = "lazy_binop"; tag = -1 }))
      in
      let expr = read_expr state in
      let cont = read_cont state in
      LazyBinOp (loc, env, lazy_binop, expr, cont)
  | 12 ->
      let loc = read_loc state in
      let env = read_env state in
      let name = read_string state in
      let arg_exprs = read_list read_expr state in
      let arg_values = read_list read_value state in
      let cont = read_cont state in
      PerformArgs (loc, env, name, arg_exprs, arg_values, cont)
  | 13 ->
      let cont1 = read_cont state in
      let cont2 = read_cont state in
      Compose (cont1, cont2)
  | 14 ->
      let env = read_env state in
      let values = read_list read_value state in
      let exprs = read_list read_expr state in
      let cont = read_cont state in
      EvalListLiteral (env, values, exprs, cont)
  | 15 ->
      let env = read_env state in
      let name = read_string state in
      let built_map =
        RecordMap.of_seq
          (List.to_seq
             (read_list
                (fun state ->
                  let name = read_string state in
                  let value = read_value state in
                  (name, value))
                state))
      in
      let bindings =
        read_list
          (fun state ->
            let name = read_string state in
            let expr = read_expr state in
            (name, expr))
          state
      in
      let cont = read_cont state in
      EvalRecordLiteral (env, name, built_map, bindings, cont)
  | 16 ->
      let loc = read_loc state in
      let env = read_env state in
      let branches =
        read_list
          (fun state ->
            let pattern = read_pattern state in
            let expr = read_expr state in
            (pattern, expr))
          state
      in
      let cont = read_cont state in
      EvalMatch (loc, env, branches, cont)
  | tag -> raise (DeserializationError (InvalidTag { ty = "cont"; tag }))

let write_environment_delta state previous { Syntax.variables } =
  write_optional write_int state previous;

  write_int state (NameMap.cardinal variables);
  let write_variable name value =
    write_string state name;
    write_value state value
  in

  NameMap.iter write_variable variables

let read_environment_delta previous state =
  let count = read_int state in
  let variables =
    Seq.init count (fun _ ->
        let name = read_string state in
        let value = read_value state in
        (name, value))
  in
  (previous, { variables = NameMap.of_seq variables })

let read_environment_deltas state =
  let rec go () =
    match read_bool state with
    | false -> []
    | true ->
        let previous = read_optional read_int state in
        let delta = read_environment_delta previous state in
        delta :: go ()
  in
  let deltas = go () in
  deltas

type serialization_target =
  | SerializeEnv of env
  | SerializeCont : ('a, 'r) Eval.cont -> serialization_target

let serialize target out_channel =
  let state =
    {
      environments = [];
      environments_to_be_written = Queue.create ();
      env_count = 0;
      out_channel;
    }
  in

  let main_index =
    match target with
    | SerializeEnv env -> register_env_index state env
    | _ -> -1
  in

  let rec write_environments () =
    match Queue.take_opt state.environments_to_be_written with
    | None -> write_bool state false
    | Some (_, previous, delta) ->
        write_bool state true;
        write_environment_delta state previous delta;
        write_environments ()
  in
  write_byte state 0xF1;
  write_byte state 0x05;
  write_byte state 0xA0;
  match target with
  | SerializeEnv _ ->
      write_environments ();
      write_int state main_index
  | SerializeCont cont ->
      write_cont state cont;
      write_environments ()

type 'r cont_result =
  | Value : value cont_result
  | EnvValue : (env * value) cont_result

type 'a deserialization_target =
  | DeserializeEnv : env deserialization_target
  | DeserializeCont :
      'r cont_result
      -> (value, 'r) Eval.cont deserialization_target

let deserialize : type a. a deserialization_target -> in_channel -> a =
 fun target in_channel ->
  let state = { in_channel } in

  let magic_byte1 = read_byte state in
  let magic_byte2 = read_byte state in
  let magic_byte3 = read_byte state in
  begin
    match (magic_byte1, magic_byte2, magic_byte3) with
    | 0xF1, 0x05, 0xA0 -> ()
    | _ -> raise (DeserializationError NotAFloraEnvironment)
  end;

  let cont =
    match target with
    | DeserializeEnv -> None
    | DeserializeCont _ -> Some (read_cont state)
  in

  let env_deltas = read_environment_deltas state in

  let env_deltas = Array.of_list env_deltas in

  let filled_envs = Array.map (fun _ -> None) env_deltas in

  let rec fill_env index =
    match filled_envs.(index) with
    | Some env -> env
    | None ->
        let previous, read_delta = env_deltas.(index) in

        let previous_env =
          match previous with
          | Some previous -> fill_env previous
          | None -> Syntax.empty_env
        in

        let merge_contents Syntax.{ variables = variables1 }
            Syntax.{ variables = variables2 } =
          {
            Syntax.variables =
              NameMap.union (fun _ _ x -> Some x) variables1 variables2;
          }
        in

        let previous =
          match previous with
          | None -> None
          | Some index -> Some (fill_env index)
        in

        let delta =
          { Syntax.variables = NameMap.map fill_value read_delta.variables }
        in

        let env =
          {
            contents = merge_contents previous_env.contents delta;
            previous;
            delta;
          }
        in

        filled_envs.(index) <- Some env;
        env
  and fill_value = function
    | Nil -> Syntax.Nil
    | Number f -> Syntax.Number f
    | String str -> Syntax.String str
    | Bool bool -> Syntax.Bool bool
    | List values -> Syntax.List (List.map fill_value values)
    | Record values -> Syntax.Record (RecordMap.map fill_value values)
    | Closure (index, params, expr) ->
        let env = lazy (fill_env index) in
        Closure (env, params, expr)
    | Primop name -> Syntax.Primop name
    | Continuation cont -> Syntax.Continuation (Obj.magic cont)
  in

  match target with
  | DeserializeEnv ->
      let main_index = read_int state in
      let env = fill_env main_index in
      trace_deserialize
        (lazy
          ("environment variables: ["
          ^ String.concat ", "
              (List.map fst (NameMap.bindings env.contents.variables))
          ^ "]"));
      env
  | DeserializeCont result_type ->
      let with_r : type r. r cont_result -> (value, r) Eval.cont =
       fun result ->
        let rec fill_value_cont : read_cont -> (value, r) Eval.cont = function
          | Done -> begin
              match result with
              | Value -> Done
              | EnvValue ->
                  raise
                    (DeserializationError
                       (ContTypeError
                          {
                            expected = "(env, value) cont";
                            actual = "value cont";
                          }))
            end
          | EvalAppFun (loc, env, exprs, cont) ->
              let env = fill_env env in
              let cont = fill_value_cont cont in
              EvalAppFun (loc, env, exprs, cont)
          | EvalAppArgs
              ((closure_env, params, body), env, arguments, argument_exprs, cont)
            ->
              let closure_env = fill_env closure_env in
              let env = fill_env env in
              let arguments = List.map fill_value arguments in
              let cont = fill_value_cont cont in
              EvalAppArgs
                ( (closure_env, params, body),
                  env,
                  arguments,
                  argument_exprs,
                  cont )
          | EvalAppPrimop (primop, env, loc, arg_values, arg_exprs, cont) ->
              let env = fill_env env in
              let arg_values = List.map fill_value arg_values in
              let cont = fill_value_cont cont in
              EvalAppPrimop (primop, env, loc, arg_values, arg_exprs, cont)
          | IfCont (loc, env, then_branch, else_branch, cont) ->
              let env = fill_env env in
              let cont = fill_value_cont cont in
              IfCont (loc, env, then_branch, else_branch, cont)
          | WithEnv (env, cont) ->
              let env = fill_env env in
              let cont = fill_value_env_cont cont in
              WithEnv (env, cont)
          | IgnoreEnv _ ->
              raise
                (DeserializationError
                   (ContTypeError
                      { expected = "value cont"; actual = "(value, env) cont" }))
          | EvalSequence (env, statements, cont) ->
              let env = fill_env env in
              let cont = fill_value_env_cont cont in
              EvalSequence (env, statements, cont)
          | BindValue (env, name, statements, cont) ->
              let env = fill_env env in
              let cont = fill_value_env_cont cont in
              BindValue (env, name, statements, cont)
          | StrictBinOp1 (loc, env, strict_binop, expr, cont) ->
              let env = fill_env env in
              let cont = fill_value_cont cont in
              StrictBinOp1 (loc, env, strict_binop, expr, cont)
          | StrictBinOp2 (loc, value, strict_binop, cont) ->
              let value = fill_value value in
              let cont = fill_value_cont cont in
              StrictBinOp2 (loc, value, strict_binop, cont)
          | LazyBinOp (loc, env, lazy_binop, expr, cont) ->
              let env = fill_env env in
              let cont = fill_value_cont cont in
              LazyBinOp (loc, env, lazy_binop, expr, cont)
          | PerformArgs (loc, env, name, exprs, values, cont) ->
              let env = fill_env env in
              let values = List.map fill_value values in
              let cont = fill_value_cont cont in
              PerformArgs (loc, env, name, exprs, values, cont)
          | Compose (cont1, cont2) ->
              let cont1 = fill_value_cont cont1 in
              begin
                match result with
                | Value ->
                    let cont2 = fill_value_cont cont2 in
                    Compose (cont1, cont2)
                | EnvValue ->
                    let cont2 = fill_value_env_cont cont2 in
                    Compose (cont1, cont2)
              end
          | EvalListLiteral (env, values, exprs, cont) ->
              let env = fill_env env in
              let values = List.map fill_value values in
              let cont = fill_value_cont cont in
              EvalListLiteral (env, values, exprs, cont)
          | EvalRecordLiteral (env, name, built_map, bindings, cont) ->
              let env = fill_env env in
              let built_map = NameMap.map fill_value built_map in
              let cont = fill_value_cont cont in
              EvalRecordLiteral (env, name, built_map, bindings, cont)
          | EvalMatch (loc, env, branches, cont) ->
              let env = fill_env env in
              let cont = fill_value_cont cont in
              EvalMatch (loc, env, branches, cont)
        and fill_value_env_cont : read_cont -> (env * value, r) Eval.cont =
          function
          | Done -> begin
              match result with
              | EnvValue -> Done
              | Value ->
                  raise
                    (DeserializationError
                       (ContTypeError
                          {
                            expected = "value cont";
                            actual = "(env, value) cont";
                          }))
            end
          | IgnoreEnv cont ->
              let cont = fill_value_cont cont in
              IgnoreEnv cont
          | EvalSequence (env, statements, cont) ->
              let env = fill_env env in
              let cont = fill_value_env_cont cont in
              EvalSequence (env, statements, cont)
          | _ ->
              raise
                (DeserializationError
                   (ContTypeError
                      { expected = "(env * value) cont"; actual = "value cont" }))
        in
        let cont =
          match cont with
          | None -> Util.panic __LOC__ "Unreachable: unread continuation"
          | Some cont -> cont
        in
        fill_value_cont cont
      in
      with_r result_type
