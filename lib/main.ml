open Ast
open Tableau
open Utils

(** [parse s] is the AST of the string [s]. *)
let parse (s : string) : formula =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast

(** [is_closed t] is whether the tableau [t] is closed. *)
let rec is_closed = function
  | Branch (_, _, l, r) -> is_closed l && is_closed r
  | Closed _ -> true
  | Open -> false

(** [is_satisfiable f] is whether the formula [f] is satisfiable. *)
let is_satisfiable (f : formula) : bool =
  let tableau = expand (Env.empty, 0) f in
  not @@ is_closed tableau

(** [is_valid f] is whether the formula [f] is valid. *)
and is_valid (f : formula) : bool =
  let tableau = expand (Env.empty, 0) (Not f) in
  is_closed tableau

(** [string_of_tableau] converts a tableau [t] to its string representation *)
let rec string_of_tableau t =
  let buffer = Buffer.create 1024 in
  Buffer.add_string buffer "Tableau\n";

  let rec string_of_tableau_aux node prefix is_last =
    match node with
    | Branch (env, f, left, right) ->
      Buffer.add_string buffer (prefix ^ (if is_last then "└── " else "├── ") ^ string_of_formula f ^ " " ^ string_of_env env ^ "\n");
      let new_prefix = prefix ^ (if is_last then "    " else "│   ") in
      string_of_tableau_aux left new_prefix false;
      string_of_tableau_aux right new_prefix true
    | Closed env ->
      Buffer.add_string buffer (prefix ^ (if is_last then "└── " else "├── ") ^ "⊥ " ^ string_of_env env ^ "\n")
    | Open -> ()
  in

  string_of_tableau_aux t "" true;
  Buffer.contents buffer

(** [string_of_env env] converts an environment [env] to its string representation *)
and string_of_env (env, i) =
  "[" ^ string_of_int i ^ "]" ^ "{" ^ String.concat ", " (List.map (fun x -> x) (Env.elements env)) ^ "}"