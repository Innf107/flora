open Syntax
open Util
open MenhirLib.Convert

let eval_string env program_text =
  let parse = Simplified.traditional2revised 
    Parser.main
  in
  let syntax = parse (Lexer.run ~filename:"<interactive>" program_text) in
  print_endline (pretty_expr syntax);
  todo __LOC__
