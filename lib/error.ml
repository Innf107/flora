type t =
  | LexicalError of Lexer.lexical_error
  | ParseError (* TODO: Carry the source location of the parse error *)
  | EvalError of Loc.t * Eval.eval_error

let handle ~handler cont =
  try cont () with
  | Lexer.LexicalError error -> handler (LexicalError error)
  | Parser.Error -> handler ParseError
  | Eval.EvalError (loc, error) -> handler (EvalError (loc, error))

let pretty = function
  | LexicalError error -> begin
      match error with
      | UnexpectedEOF -> "Lexical error: Unexpected end of file"
      | UnterminatedString -> "Lexical error: Unterminated string literal"
      | UnexpectedChar char ->
          Printf.sprintf "Lexical error: Unexpected character: '%c'" char
    end
  | ParseError -> "Syntax error"
  | EvalError (loc, error) -> Loc.pretty loc ^ ": " ^ begin match error with 
    | VarNotFound name -> "Unbound variable: '" ^ name ^ "'" 
    | TryingToCallNonFunction value -> "Trying to call non-function value: " ^ Syntax.pretty_value value
    | IncorrectNumberOfArgs { expected; actual } -> 
        "Incorrect number of arguments passed to function.\n"
      ^ "    Expected: " ^ string_of_int expected ^ "\n"
      ^ "      Actual: " ^ string_of_int actual
    | InvalidOperatorArgs { operator; expected; actual } -> 
        "Invalid arguments to primitive operator " ^ operator ^ ".\n"
      ^ "    Expected arguments of the form: (" ^ String.concat ", " expected ^ ")\n"
      ^ "                            Actual: (" ^ String.concat ", " (List.map (function None -> "_" | Some x -> Syntax.pretty_value x) actual) ^ ")"
  end
