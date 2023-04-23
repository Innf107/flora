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

let () =
  let initial_options = { file_to_run = None } in
  let options = parse_args initial_options (List.tl (Array.to_list Sys.argv)) in

  match options.file_to_run with
  | None -> Util.todo __LOC__
  | Some file -> begin
      let contents =
        In_channel.with_open_text "test.flora" In_channel.input_all
      in
      match
        Driver.eval_string Syntax.{ variables = NameMap.empty } contents
      with
      | _env, value -> print_endline (Syntax.pretty_value value)
      | exception Lexer.LexicalError err -> begin
          match err with
          | UnexpectedEOF ->
              print_endline "Lexical error: Unexpected end of file"
          | UnterminatedString ->
              print_endline "Lexical error: Unterminated string literal"
          | UnexpectedChar char ->
              Printf.printf "Lexical error: Unexpected character: '%c'\n" char
        end
    end
