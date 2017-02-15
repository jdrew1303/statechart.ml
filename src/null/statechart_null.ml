open Statechart_datamodel
open Statechart_executable

let parse str =
  let lexbuf = Lexing.from_string str in
  try
    let name = Statechart_null_parser.main Statechart_null_lexer.token lexbuf in
    Program (complex_expr `is_active [string_expr name])
  with
  | _ ->
    Error []
