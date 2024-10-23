type term = Var of string | Fun of string * term list

type formula =
  | Not of formula
  | And of formula * formula
  | Or of formula * formula
  | Implies of formula * formula
  | Predicate of string * term list
  | Iff of formula * formula
  | Forall of string * formula
  | Exists of string * formula