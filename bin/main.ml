open Flora

type flora_options = {
  file_to_run : string option;
  write_env_to : out_channel option;
  read_env_from : in_channel option;
}

let usage =
  "Usage: flora [options] [file]\n" ^ "\n" ^ "    Options\n"
  ^ "      --help             Show this message\n"
  ^ "      --write-env [FILE] Write the final evaluation environment to FILE\n"
  ^ "      --read-env [FILE]  Read the initial evaluation environment from FILE"

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
  | "--write-env" :: file :: rest ->
      let out_channel = open_out file in
      parse_args { options with write_env_to = Some out_channel } rest
  | [ "--write-env" ] -> error_usage "Option '--write-env' expects an argument"
  | "--read-env" :: file :: rest ->
      let in_channel = open_in file in
      parse_args { options with read_env_from = Some in_channel } rest
  | [ "--read-env" ] -> error_usage "Option '--read-env' expects an argument"
  | arg :: _ when String.starts_with ~prefix:"-" arg ->
      error_usage ("Invalid option: '" ^ arg ^ "'")
  | arg :: rest -> (
      match options.file_to_run with
      | None -> parse_args { options with file_to_run = Some arg } rest
      | Some _ -> error_usage "Too many arguments")

let run_repl env options =
  let rec go env =
    match Bestline.bestline "\x1b[95m\x1b[38;2;255;0;255mλ>\x1b[0m " with
    | None -> env
    | Some line ->
        Error.handle
          ~handler:(fun error ->
            prerr_endline ("ERROR: " ^ Error.pretty error);
            go env)
          begin
            fun () ->
              let env, result = Driver.eval_string ~filename:None env line in

              (* TODO: Print this differently if the output is not a tty *)
              print_endline ("- " ^ Syntax.pretty_value result);
              go env
          end
  in
  go env

let () =
  let initial_options =
    { file_to_run = None; write_env_to = None; read_env_from = None }
  in
  let options = parse_args initial_options (List.tl (Array.to_list Sys.argv)) in

  let initial_env =
    match options.read_env_from with
    | None -> Syntax.empty_env
    | Some in_channel -> Serialize.deserialize_env in_channel
  in

  match options.file_to_run with
  | None -> 
    let env = run_repl initial_env options in
    begin match options.write_env_to with
    | None -> ()
    | Some out_channel ->
      Serialize.serialize_env out_channel env
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
            let env, value =
              Driver.eval_string ~filename:(Some file) initial_env
                contents
            in
            begin match options.write_env_to with
            | None -> ()
            | Some out_channel ->
              Serialize.serialize_env out_channel env
            end;        
            print_endline (Syntax.pretty_value value)
        end
    end
