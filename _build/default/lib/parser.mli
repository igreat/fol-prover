
(* The type of tokens. *)

type token = 
  | TERM_ID of (string)
  | RPAREN
  | PREDICATE_ID of (string)
  | OR
  | NOT
  | LPAREN
  | IMPLIES
  | IFF
  | FORALL
  | EXISTS
  | EOF
  | AND

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val prog: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Ast.formula)
