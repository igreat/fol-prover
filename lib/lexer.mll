{
open Parser
}

let white = [' ' '\t' '\n' '\r']+
let digit = ['0'-'9']+
let upper = ['A'-'Z']
let lower = ['a'-'z']
let predicate_id = upper (upper | lower | digit)*
let term_id = (lower | upper | digit | '_')+
let separator = white | ','

rule read =
  parse 
  | separator { read lexbuf }
  | "forall" { FORALL }
  | "exists" { EXISTS }
  | "not" { NOT }
  | "->" { IMPLIES }
  | "<->" { IFF }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "and" { AND }
  | "or" { OR }
  | predicate_id { PREDICATE_ID (Lexing.lexeme lexbuf) }
  | term_id { TERM_ID (Lexing.lexeme lexbuf) }
  | eof { EOF }


