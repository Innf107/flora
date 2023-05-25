let usage = {|Usage: florainspect tokens FILE|}

let fail_usage message =
  prerr_endline (message ^ "\n\n" ^ usage);
  exit 1

type command = Tokens of { file : string }

let parse_tokens = function
  | option :: _ when String.starts_with ~prefix:"-" option ->
      fail_usage ("Invalid option: " ^ option)
  | [ file ] -> Tokens { file }
  | _ :: _ -> fail_usage "tokens: Too many arguments"
  | [] -> fail_usage "tokens: Missing required argument"

let parse_args = function
  | "tokens" :: rest -> parse_tokens rest
  | command :: _ -> fail_usage ("Invalid command: " ^ command)
  | [] -> fail_usage "Missing command"

let () =
  match parse_args (List.tl (Array.to_list Sys.argv)) with
  | Tokens { file } ->
      let contents = In_channel.with_open_bin file In_channel.input_all in
      let next_token = Flora.Lexer.run ~filename:file contents in
      Flora.Error.handle
        ~handler:(fun err -> prerr_endline ("ERROR: " ^ Flora.Error.pretty err))
        begin
          fun () ->
            let rec go () =
              match next_token () with
              | EOF, _, _ -> print_endline "EOF"
              | token, _, _ ->
                  let str =
                    match token with
                    | Flora.Parser.EOF -> "EOF"
                    | IDENT ident -> "ident(" ^ ident ^ ")"
                    | NUMBER number -> "number(" ^ Float.to_string number ^ ")"
                    | STRING str -> "string(" ^ str ^ ")"
                    | LPAREN -> "("
                    | RPAREN -> ")"
                    | LBRACE -> "{"
                    | RBRACE -> "}"
                    | LBRACKET -> "["
                    | RBRACKET -> "]"
                    | LET -> "let"
                    | IF -> "if"
                    | THEN -> "then"
                    | ELSE -> "else"
                    | NIL -> "nil"
                    | TRUE -> "true"
                    | FALSE -> "false"
                    | PERFORM -> "perform"
                    | HANDLE -> "handle"
                    | MATCH -> "match"
                    | AS -> "as"
                    | EQUALS -> "="
                    | SEMI -> ";"
                    | COMMA -> ","
                    | LAMBDA -> "λ"
                    | ARROW -> "->"
                    | LESS -> "<"
                    | LESSEQUAL -> "<="
                    | DOUBLEEQUAL -> "=="
                    | NOTEQUAL -> "!="
                    | GREATEREQUAL -> ">="
                    | GREATER -> ">"
                    | PLUS -> "+"
                    | MINUS -> "-"
                    | STAR -> "*"
                    | SLASH -> "/"
                    | COLON -> ":"
                    | TILDE -> "~"
                    | AND -> "&&"
                    | OR -> "||"
                    | PIPE -> "|"
                  in
                  print_endline str;
                  go ()
            in
            go ()
        end
