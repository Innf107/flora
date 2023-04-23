open Syntax
open Util
open MenhirLib.Convert

let eval_string env program_text =
  let parse = Simplified.traditional2revised 
    Parser.main
  in
  let syntax = parse (Lexer.run ~filename:"<interactive>" program_text) in
  let result_env, result = Eval.eval env syntax in
  result_env, result
