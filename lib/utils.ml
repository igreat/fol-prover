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
  | Not f -> "¬" ^ string_of_formula f
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
