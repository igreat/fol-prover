open Ast
open Tableau

(** [parse s] is the AST of the string [s]. *)
let parse (s : string) : formula =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast

(** [is_closed t] is whether the tableau [t] is closed. *)
let rec is_closed = function
  | Branch (_, _, l, r) -> is_closed l && is_closed r
  | Closed _ -> true
  | Leaf -> false

(** [is_satisfiable f] is whether the formula [f] is satisfiable. *)
let is_satisfiable (f : formula) : bool =
  let tableau = expand (Env.empty, 0) f in
  not @@ is_closed tableau

(** [is_valid f] is whether the formula [f] is valid. *)
and is_valid (f : formula) : bool =
  let tableau = expand (Env.empty, 0) (Not f) in
  is_closed tableau
