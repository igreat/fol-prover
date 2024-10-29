open Ast
open Utils
module Env = Set.Make (String)

type env = Env.t
type t = Branch of env * formula * t * t | Leaf

let get_env = function Branch (env, _, _, _) -> env | Leaf -> Env.empty

(** [expand f] expands [f] into a tableau. *)
let rec expand env = function
  | Not f -> expand_not env f
  | And (f1, f2) -> expand_and env f1 f2
  | Or (f1, f2) -> expand_or env f1 f2
  | Implies (f1, f2) -> expand_implies env f1 f2
  | Iff (f1, f2) -> expand_iff env f1 f2
  | Forall (x, f) -> expand_forall env x f
  | Exists (x, f) -> expand_exists env x f
  | Predicate _ as pred ->
      Branch (Env.add (string_of_formula pred) env, pred, Leaf, Leaf)

and expand_not env = function
  | Not f -> expand env f
  | And (f1, f2) -> expand env (Or (Not f1, Not f2))
  | Or (f1, f2) -> expand env (And (Not f1, Not f2))
  | Implies (f1, f2) -> expand env (And (f1, Not f2))
  | Iff (f1, f2) -> expand env (Or (And (f1, Not f2), And (Not f1, f2)))
  | Forall (x, f) -> expand env (Exists (x, Not f))
  | Exists (x, f) -> expand env (Forall (x, Not f))
  | Predicate _ as pred ->
      Branch
        (Env.add (string_of_formula (Not pred)) env, Not pred, Leaf, Leaf)

(* here is where I'll likely need to share envs or maybe in join *)
and expand_and env f1 f2 =
  Branch (env, And (f1, f2), join (expand env f1) f2, Leaf)

and expand_or env f1 f2 = Branch (env, Or (f1, f2), expand env f1, expand env f2)

and expand_implies env f1 f2 =
  Branch (env, Implies (f1, f2), expand env (Not f1), expand env f2)

and expand_iff env f1 f2 =
  Branch
    ( env,
      Iff (f1, f2),
      expand env (And (f1, Not f2)),
      expand env (And (Not f1, f2)) )

and expand_forall _ _ _ = Leaf (* TODO *)

(* maybe have special prefix # as a new thingy, and then just give it a number, that way I can go indefinitely *)
(* introduce a new letter *)
and expand_exists _ _ _ = Leaf (* TODO *)

(** [join t1 t2] will extend all leaves of [t1] with [t2]. *)
and join t f =
  match t with
  | Branch (env, f', Leaf, Leaf) -> Branch (env, f', expand env f, Leaf)
  | Branch (env, f', Leaf, r1) -> Branch (env, f', Leaf, join r1 f)
  | Branch (env, f', l1, Leaf) -> Branch (env, f', join l1 f, Leaf)
  | Branch (env, f', l1, r1) -> Branch (env, f', join l1 f, join r1 f)
  | Leaf -> failwith "unexpected Leaf"



(* * [string_of_tableau t] converts a tableau [t] to its string representation
let string_of_tableau t =
  let buffer = Buffer.create 1024 in

  let rec string_of_tableau_aux node prefix is_last =
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
  Buffer.contents buffer *)