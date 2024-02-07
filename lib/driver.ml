open Syntax
open Util
open MenhirLib.Convert.Simplified

exception ParseError of Loc.t

let eval_string ~filename env program_text =
  let lexbuf = Sedlexing.Utf8.from_string program_text in
  Sedlexing.set_filename lexbuf
    (Option.value ~default:"<<interactive>>" filename);

  let lexer () =
    let token = Lexer.lex lexbuf in
    let start, end_ = Sedlexing.lexing_positions lexbuf in
    (token, start, end_)
  in
  let syntax =
    try traditional2revised Parser.main lexer with
    | Parser.Error ->
        raise
          (ParseError
             (let start, end_ = Sedlexing.lexing_positions lexbuf in
              Loc.from_positions start end_))
  in

  Eval.eval_statements env syntax
