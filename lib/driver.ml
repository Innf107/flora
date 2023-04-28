open Syntax
open Util
open MenhirLib.Convert

let eval_string ~filename env program_text =
  let parse = Simplified.traditional2revised 
    Parser.main
  in
  let filename = Option.value ~default:"<interactive>" filename in

  let syntax = parse (Lexer.run ~filename program_text) in
  let result_env, result = Eval.eval env syntax in
  result_env, result
