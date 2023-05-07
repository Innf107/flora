type void

type ('a, 'b) mandatory =
  | Set of 'a
  | Unset of 'b

type 'a view_env_options_validation = {
  env_file : (string, 'a) mandatory;
  abbreviate : int;
}

type view_env_options = void view_env_options_validation
type command = ViewEnv of view_env_options

let get : ('a, void) mandatory -> 'a = function
  | Set a -> a
  | Unset x -> raise (Failure "absurd")

let usage =
  {|Usage: floraedit view-env [OPTIONS] FILE

View and edit flora environment files

Run floraedit COMMAND --help for options|}

let view_edit_usage =
  {|Usage: floraedit view-env [OPTIONS] FILE

View contents of environment files

OPTIONS
  --abbreviate COUNT    Abbreviate values after COUNT characters. 
                        Passing -1 disables abbreviation. Default: 100 
|}

let fail_generic_usage message =
  prerr_endline message;
  prerr_newline ();
  prerr_endline usage;
  exit 1

let fail_view_edit_usage message =
  prerr_endline message;
  prerr_newline ();
  prerr_endline view_edit_usage;
  exit 1

let rec parse_view_env options = function
  | [] -> options
  | "--abbreviate" :: count :: rest ->
      let count =
        match int_of_string_opt count with
        | None ->
            fail_view_edit_usage
              "Option --abbreviate expects an integer argument"
        | Some count -> count
      in
      parse_view_env { options with abbreviate = count } rest
  | [ "--abbreviate" ] ->
      fail_view_edit_usage "Option --abbreviate expects an argument"
  | flag :: _ when String.starts_with ~prefix:"-" flag ->
      fail_view_edit_usage ("Invalid flag: " ^ flag)
  | file :: rest ->
      let options =
        match options.env_file with
        | Set _ -> fail_view_edit_usage "Too many arguments"
        | Unset () -> { options with env_file = Set file }
      in
      parse_view_env options rest

let parse_args : string list -> command = function
  | [] -> fail_generic_usage "Missing command"
  | flag :: _ when String.starts_with ~prefix:"-" flag ->
      fail_generic_usage ("Invalid flag: " ^ flag)
  | "view-env" :: args ->
      let empty_options = { env_file = Unset (); abbreviate = 100 } in
      let { env_file; abbreviate } = parse_view_env empty_options args in
      let env_file =
        match env_file with
        | Set env_file -> Set env_file
        | Unset () -> fail_generic_usage "Missing required argument"
      in
      ViewEnv { env_file; abbreviate }
  | command :: _ -> fail_generic_usage ("Invalid command: " ^ command)

let print_environment_contents options env_contents =
  print_endline "{";
  let print_entry name value =
    let value_string = Flora.Syntax.pretty_value value in

    let value_string =
      if options.abbreviate > 0 && String.length value_string > options.abbreviate then
        String.sub value_string 0 options.abbreviate ^ "..."
      else value_string
    in

    print_endline ("    " ^ name ^ " = " ^ value_string)
  in
  Flora.Syntax.(NameMap.iter print_entry env_contents.variables);
  print_endline "}"

let run_view_env options =
  let env =
    In_channel.with_open_bin (get options.env_file)
      Flora.Serialize.(deserialize DeserializeEnv)
  in

  print_environment_contents options env.contents

let () =
  let command = parse_args (List.tl (Array.to_list Sys.argv)) in

  match command with ViewEnv options -> run_view_env options
