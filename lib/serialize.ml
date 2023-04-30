open Syntax

let deserialize_category, trace_deserialize =
  Trace.make ~flag:"deserialize" ~prefix:"DESERIALIZE"

type deserialization_error =
  | EOF
  | InvalidTag of {
      ty : string;
      tag : int;
    }

exception DeserializationError of deserialization_error

type write_state = {
  (* TODO: Would be nice to use a map here *)
  mutable environments : (int * env_contents) list;
  mutable env_count : int;
  environments_to_be_written : (int * int option * env_contents) Queue.t;
  out_channel : out_channel;
}

type read_state = { in_channel : in_channel }

type read_value =
  (* We might not have read the entire environment yet so this needs to be 'int' *)
  | Closure of int * name list * expr
  (* The rest is just boilerplate copied from the definition of Syntax.value *)
  | Nil
  | Number of float
  | String of string
  | Bool of bool
  | List of read_value list

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
        match In_channel.input state.in_channel buffer 1 7 with
        | 7 ->
            Int64.to_int
              (Int64.shift_right_logical (Bytes.get_int64_le buffer 0) 1)
        | _ -> raise (DeserializationError EOF)
      end

let write_float state float =
  let buffer = Bytes.create 8 in
  Bytes.set_int64_le buffer 0 (Int64.bits_of_float float);
  Out_channel.output_bytes state.out_channel buffer

let read_float state =
  let buffer = Bytes.create 8 in
  match In_channel.input state.in_channel buffer 0 8 with
  | 8 -> Int64.float_of_bits (Bytes.get_int64_le buffer 0)
  | _ -> raise (DeserializationError EOF)

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
  match In_channel.input state.in_channel bytes 0 size with
  | written when written = size -> String.of_bytes bytes
  | _ -> raise (DeserializationError EOF)

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
  | (`Add : binop) -> write_byte state 0
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
      write_list write_string state params;
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

and write_statement state = function
  | RunExpr expr ->
      write_byte state 0;
      write_expr state expr
  | Let (loc, name, expr) ->
      write_byte state 1;
      write_loc state loc;
      write_string state name;
      write_expr state expr
  | LetFun (loc, name, params, body) ->
      write_byte state 2;
      write_loc state loc;
      write_string state name;
      write_list write_string state params;
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
      let params = read_list read_string state in
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
  | tag -> raise (DeserializationError (InvalidTag { ty = "expr"; tag }))

and read_statement state =
  match read_byte state with
  | 0 ->
      let loc = read_loc state in
      let name = read_string state in
      let expr = read_expr state in
      Let (loc, name, expr)
  | 1 ->
      let expr = read_expr state in
      RunExpr expr
  | tag -> raise (DeserializationError (InvalidTag { ty = "statement"; tag }))

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
      write_list write_string state parameters;
      write_expr state body

let rec read_value state : read_value =
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
      let params = read_list read_string state in
      let body = read_expr state in
      Closure (env, params, body)
  | tag -> raise (DeserializationError (InvalidTag { ty = "value"; tag }))

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
  let main_index = read_int state in
  (deltas, main_index)

let serialize_env out_channel env =
  let state =
    {
      environments = [];
      environments_to_be_written = Queue.create ();
      env_count = 0;
      out_channel;
    }
  in
  let main_index = register_env_index state env in

  let rec write_environments () =
    match Queue.take_opt state.environments_to_be_written with
    | None -> ()
    | Some (_, previous, delta) ->
        write_bool state true;
        write_environment_delta state previous delta;
        write_environments ()
  in
  write_environments ();
  write_bool state false;
  write_int state main_index

let deserialize_env in_channel =
  let state = { in_channel } in

  let env_deltas, main_index = read_environment_deltas state in

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
    | Closure (index, params, expr) ->
        let env = fill_env index in
        Closure (lazy env, params, expr)
  in

  let env = fill_env main_index in
  trace_deserialize
    (lazy
      ("environment variables: ["
      ^ String.concat ", "
          (List.map fst (NameMap.bindings env.contents.variables))
      ^ "]"));
  env
