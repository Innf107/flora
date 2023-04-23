open Flora

let () =
  let contents = In_channel.with_open_text "test.flora" In_channel.input_all in
  match Driver.eval_string Syntax.{ variables = NameMap.empty } contents with
  | _ -> ()
  | exception Lexer.LexicalError err -> begin
      match err with
      | UnexpectedEOF -> print_endline "Lexical error: Unexpected end of file"
      | UnexpectedChar char ->
          Printf.printf "Lexical error: Unexpected character: '%c'" char
    end
