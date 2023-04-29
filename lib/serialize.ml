open Syntax

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
  let buffer = Bytes.create 8 in
  Bytes.set_int64_le buffer 0 (Int64.of_int int);
  Out_channel.output_bytes state.out_channel buffer

let read_int state =
  let buffer = Bytes.create 8 in
  match In_channel.input state.in_channel buffer 0 8 with
  | 8 -> Int64.to_int (Bytes.get_int64_le buffer 0)
  | _ -> raise (DeserializationError EOF)

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
      write_env state env;
      write_list write_string state parameters;
      Util.todo __LOC__

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
      let body = Util.todo __LOC__ in
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
        Closure (env, params, expr)
  in

  fill_env main_index
