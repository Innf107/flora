open Flora

type flora_options = { file_to_run : string option }

let usage =
  "Usage: flora [options] [file]\n" ^ "\n" ^ "    Options\n"
  ^ "      --help   Show this message"

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
  | arg :: _ when String.starts_with ~prefix:"-" arg ->
      error_usage ("Invalid option: '" ^ arg ^ "'")
  | arg :: rest -> (
      match options.file_to_run with
      | None -> parse_args { options with file_to_run = Some arg } rest
      | Some _ -> error_usage "Too many arguments")

let run_repl options =
  let env = Syntax.{ variables = NameMap.empty } in
  let rec go env =
    match Bestline.bestline "\x1b[95m\x1b[38;2;255;0;255mλ>\x1b[0m " with
    | None -> ()
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
  let initial_options = { file_to_run = None } in
  let options = parse_args initial_options (List.tl (Array.to_list Sys.argv)) in

  match options.file_to_run with
  | None -> run_repl options
  | Some file -> begin
      Error.handle
        ~handler:(fun error ->
          prerr_endline ("ERROR: " ^ Error.pretty error);
          exit 1)
        begin
          fun () ->
            let contents =
              In_channel.with_open_text "test.flora" In_channel.input_all
            in
            let _env, value =
              Driver.eval_string ~filename:(Some"test.flora") Syntax.{ variables = NameMap.empty } contents
            in
            print_endline (Syntax.pretty_value value)
        end
    end
