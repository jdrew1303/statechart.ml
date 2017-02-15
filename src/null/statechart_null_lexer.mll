{
  open Statechart_null_parser
}

rule token = parse
| [' ' '\t' '\n']
  { token lexbuf }
| "In"
  { IN }
| '('
  { LPAREN }
| ')'
  { RPAREN }
| eof
  { EOF }
| _
  { CHAR (Lexing.lexeme lexbuf) }
