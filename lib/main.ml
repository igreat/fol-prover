open Ast

(** [parse s] is the AST of the string [s]. *)
let parse (s : string) : formula =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast