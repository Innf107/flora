open Flora

type flora_options = {
  file_to_run : string option;
  write_env_to : out_channel option;
  read_env_from : in_channel option;
  effect_channel : out_channel option;
  is_continue : string option;
}

let usage =
  {|Usage: flora [options] [file]

    Options
      --help                 Show this message
      --effects FILE         Enable effect handling. The continuation will be written to FILE
      --continue EXPR        Continue a previously suspended continuation in FILE with argument EXPR
      --write-env FILE       Write the final evaluation environment to FILE
      --read-env FILE        Read the initial evaluation environment from FILE

      --trace CATEGORY       Enable interpreter traces for debugging purposes.
                               Possible values: |}
  ^ String.concat ", " (Trace.get_categories ())

let error_usage message =
  prerr_endline message;
  prerr_newline ();
  prerr_endline usage;
  exit 1

let rec parse_args options = function
  | [] -> options
  | "--help" :: _ ->
      print_endline usage;
      exit 0
  | "--effects" :: file :: rest ->
      let out_channel = open_out file in
      parse_args { options with effect_channel = Some out_channel } rest
  | [ "--effects" ] -> error_usage "Option '--write-env' expects an argument"
  | "--continue" :: expr :: rest ->
      parse_args { options with is_continue = Some expr } rest
  | [ "--continue" ] -> error_usage "Option '--continue' expects an argument"
  | "--write-env" :: file :: rest ->
      let out_channel = open_out file in
      parse_args { options with write_env_to = Some out_channel } rest
  | [ "--write-env" ] -> error_usage "Option '--write-env' expects an argument"
  | "--read-env" :: file :: rest ->
      let in_channel = open_in file in
      parse_args { options with read_env_from = Some in_channel } rest
  | [ "--read-env" ] -> error_usage "Option '--read-env' expects an argument"
  | "--trace" :: category :: rest ->
      if Trace.try_set_enabled category true then parse_args options rest
      else error_usage ("Invalid trace category: '" ^ category ^ "'")
  | [ "--trace" ] -> error_usage "Option '--trace' expects an argument"
  | arg :: _ when String.starts_with ~prefix:"-" arg ->
      error_usage ("Invalid option: '" ^ arg ^ "'")
  | arg :: rest -> (
      match options.file_to_run with
      | None -> parse_args { options with file_to_run = Some arg } rest
      | Some _ -> error_usage "Too many arguments")

let run_repl env options =
  let rec go env =
    match
      Bestline.bestline_with_history "\x1b[95m\x1b[38;2;255;0;255mλ>\x1b[0m "
        "flora"
    with
    | None -> env
    | Some line ->
        Error.handle
          ~handler:(fun error ->
            prerr_endline ("ERROR: " ^ Error.pretty error);
            go env)
          begin
            fun () ->
              let env, result =
                match Driver.eval_string ~filename:None env line with
                | Completed (env, result) -> (env, result)
                | Suspended (effect, args, cont) -> Util.todo __LOC__
              in

              (* TODO: Print this differently if the output is not a tty *)
              print_endline ("- " ^ Syntax.pretty_value result);
              go env
          end
  in
  go env

let handle_result options result =
  let env, value =
    match result with
    | Eval.Completed (env, value) -> (env, value)
    | Suspended (effect, arguments, cont) -> begin
        match options.effect_channel with
        | None ->
            prerr_endline
              ("Unhandled effect '" ^ effect
             ^ "'. You can enable effect handling with --effects FILE");
            exit 1
        | Some out_channel ->
            Serialize.serialize out_channel (SerializeCont cont);
            close_out out_channel;
            Tojson.(
              output_to stdout
                (Tojson.obj (fun obj ->
                     Tojson.entry obj "effect" (Tojson.string effect);
                     Tojson.entry obj "arguments"
                       (Tojson.list Tojson.value arguments))));
            exit 100
      end
  in
  begin
    match options.write_env_to with
    | None -> ()
    | Some out_channel -> Serialize.serialize out_channel (SerializeEnv env)
  end;
  print_endline (Syntax.pretty_value value)

let () =
  let initial_options =
    {
      file_to_run = None;
      write_env_to = None;
      read_env_from = None;
      effect_channel = None;
      is_continue = None;
    }
  in
  let options = parse_args initial_options (List.tl (Array.to_list Sys.argv)) in

  let initial_env =
    match options.read_env_from with
    | None -> Syntax.empty_env
    | Some in_channel -> (
        try Serialize.deserialize DeserializeEnv in_channel
        with Serialize.DeserializationError err ->
          begin
            match err with
            | Serialize.EOF ->
                prerr_endline
                  "Unexpected end of file encountered while deserializing \
                   environment"
            | InvalidTag { ty; tag } ->
                prerr_endline
                  ("Error deserializing environment: Invalid tag for type '"
                 ^ ty ^ "': " ^ string_of_int tag)
            | ContTypeError { expected; actual } ->
                prerr_endline
                  ("Error deserializing environment: Continuation type error\n"
                 ^ "    expected: " ^ expected ^ "\n      actual: " ^ actual)
          end;
          exit 1)
  in

  match options.is_continue with
  | Some argument_string -> begin
      match options.file_to_run with
      | None ->
          prerr_endline
            "--continue is only valid when running a continuation file";
          exit 1
      | Some file ->
          Error.handle
            ~handler:(fun error ->
              prerr_endline ("ERROR: " ^ Error.pretty error))
            begin
              fun () ->
                let _env, argument_value =
                  match
                    Driver.eval_string ~filename:None Syntax.empty_env
                      argument_string
                  with
                  | Eval.Completed value -> value
                  | Eval.Suspended (effect, _, _) ->
                      prerr_endline
                        ("Unhandled effect in continuation argument: '" ^ effect
                       ^ "'");
                      exit 1
                in

                let cont =
                  In_channel.with_open_bin file
                    (Serialize.deserialize (DeserializeCont EnvValue))
                in
                let result = Eval.continue cont argument_value in
                handle_result options result
            end
    end
  | None -> begin
      match options.file_to_run with
      | None ->
          let env = run_repl initial_env options in
          begin
            match options.write_env_to with
            | None -> ()
            | Some out_channel ->
                Serialize.serialize out_channel (SerializeEnv env)
          end
      | Some file -> begin
          Error.handle
            ~handler:(fun error ->
              prerr_endline ("ERROR: " ^ Error.pretty error);
              exit 1)
            begin
              fun () ->
                let contents =
                  In_channel.with_open_text file In_channel.input_all
                in
                let result =
                  Driver.eval_string ~filename:(Some file) initial_env contents
                in
                handle_result options result
            end
        end
    end
