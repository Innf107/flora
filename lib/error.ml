type t =
  | LexicalError of Lexer.lexical_error
  | ParseError of Loc.t
  | EvalError of Loc.t * Eval.eval_error
  | DeserializationError of Serialize.deserialization_error

let handle ~handler cont =
  try cont () with
  | Lexer.LexicalError error -> handler (LexicalError error)
  | Driver.ParseError loc -> handler (ParseError loc)
  | Eval.EvalError (loc, error) -> handler (EvalError (loc, error))
  | Serialize.DeserializationError err -> handler (DeserializationError err)

let pretty = function
  | LexicalError error -> begin
      match error with
      | UnexpectedEOF -> "Lexical error: Unexpected end of file"
      | UnterminatedString -> "Lexical error: Unterminated string literal"
      | UnexpectedChar (char, loc) ->
          Printf.sprintf "%s: Lexical error: Unexpected character: '%s'" (Loc.pretty loc) char
    end
  | ParseError loc -> Loc.pretty loc ^ ": Syntax error"
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
    | IncorrectNumberOfHandlerArgs { effect; expected; actual } -> 
      "Incorrect number of arguments to handled effect '" ^ effect ^ ".\n"
      ^ "    Expected: " ^ string_of_int expected ^ "\n"
      ^ "      Actual: " ^ string_of_int actual
    | IncorrectNumberOfArgsToContinuation actual -> 
      "Incorrect number of arguments to handled continuation.\n"
      ^ "    Expected exactly one argument\n"
      ^ "      Actual: " ^ string_of_int actual
    | PrimopArgumentError { primop; expected; actual } ->
      "Incorrect arguments to primitive function " ^ Syntax.pretty_primop primop ^ ".\n"
    ^ "    Expected: " ^ expected ^ "\n"
    ^ "      Actual: (" ^ String.concat ", " (List.map Syntax.pretty_value actual) ^ ")"
    | NonexhaustivePatterns { scrutinee } ->
      "Non-exhaustive pattern match does not cover value '" ^ Syntax.pretty_value scrutinee ^ "'"
  end
  | DeserializationError (error) -> begin match error with 
    | Serialize.EOF -> "Error during deserialization: Unexpected end of file"
    | InvalidTag { ty; tag } ->
      
        ("Error during deserialization: Invalid tag for type '" ^ ty ^ "': " ^ string_of_int tag)
    | ContTypeError { expected; actual } ->
      
        ("Error deserializing environment: Continuation type error\n"
      ^ "    expected: " ^ expected ^ "\n"
      ^ "      actual: " ^ actual)
    | NotAFloraEnvironment ->
      "Error deserializing environment: File is not a flora environment"
    end
