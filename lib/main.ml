open Ast

(** [string_of_term t] converts a term [t] to its string representation *)
let rec string_of_term = function
  | Var x -> x
  | Fun (name, args) ->
      name ^ "(" ^ String.concat ", " (List.map string_of_term args) ^ ")"

(** [string_of_formula f] converts a formula [f] to its string representation *)
let rec string_of_formula = function
  | Predicate (p, ts) ->
      p ^ "(" ^ String.concat ", " (List.map string_of_term ts) ^ ")"
  | Not f -> "¬(" ^ string_of_formula f ^ ")"
  | And (f1, f2) ->
      "(" ^ string_of_formula f1 ^ " ∧ " ^ string_of_formula f2 ^ ")"
  | Or (f1, f2) ->
      "(" ^ string_of_formula f1 ^ " ∨ " ^ string_of_formula f2 ^ ")"
  | Implies (f1, f2) ->
      "(" ^ string_of_formula f1 ^ " ⇒ " ^ string_of_formula f2 ^ ")"
  | Iff (f1, f2) ->
      "(" ^ string_of_formula f1 ^ " ⇔ " ^ string_of_formula f2 ^ ")"
  | Forall (x, f) -> "∀" ^ x ^ ".(" ^ string_of_formula f ^ ")"
  | Exists (x, f) -> "∃" ^ x ^ ".(" ^ string_of_formula f ^ ")"

(** [string_of_tableau t] converts a tableau [t] to its string representation *)
let string_of_tableau t =
  let buffer = Buffer.create 1024 in

  let rec string_of_tableau_aux node prefix is_last =
    let open Tableau in
    match node with
    | Leaf ->
        Buffer.add_string buffer (prefix ^ (if is_last then "└── " else "├── ") ^ ("Leaf") ^ "\n")
    | Branch (f, left, right) ->
        Buffer.add_string buffer (prefix ^ (if is_last then "└── " else "├── ") ^ (string_of_formula f) ^ "\n");
        let new_prefix = prefix ^ (if is_last then "    " else "│   ") in
        string_of_tableau_aux left new_prefix false;
        string_of_tableau_aux right new_prefix true
  in

  string_of_tableau_aux t "" true;
  Buffer.contents buffer

(** [parse s] is the AST of the string [s]. *)
let parse (s : string) : formula =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast
