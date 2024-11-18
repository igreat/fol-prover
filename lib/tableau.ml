open Ast
open Utils
module Env = Set.Make (String)

type env = Env.t
type t = 
  | Branch of (env * int) * formula * t * t 
  | Closed of (env * int) (* env kept for debugging *)
  | Open

let max_constants = 10

let negate = function
  | Not f -> f
  | f -> Not f

let add_to_env (env, i) s = (Env.add s env, i)

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
    let new_env = add_to_env env (string_of_formula pred) in
    if Env.mem (string_of_formula (negate pred)) (fst env) then
      Closed new_env
    else
      Branch (new_env, pred, Open, Open)

and expand_not env = function
  | Not f -> expand env f
  | And (f1, f2) -> expand env (Or (Not f1, Not f2))
  | Or (f1, f2) -> expand env (And (Not f1, Not f2))
  | Implies (f1, f2) -> expand env (And (f1, Not f2))
  | Iff (f1, f2) -> expand env (Or (And (f1, Not f2), And (Not f1, f2)))
  | Forall (x, f) -> expand env (Exists (x, Not f))
  | Exists (x, f) -> expand env (Forall (x, Not f))
  | Predicate _ as pred ->
    let new_env = add_to_env env (string_of_formula (negate pred)) in
    if Env.mem (string_of_formula pred) (fst env) then
      Closed new_env
    else
      Branch
        (new_env, negate pred, Open, Open)

(** [expand_and f1 f2] expands [f1 and f2] into a tableau. *)
and expand_and env f1 f2 =
  Branch (env, And (f1, f2), join (expand env f1) f2, Closed env)

(** [expand_or f1 f2] expands [f1 or f2] into a tableau. *)
and expand_or env f1 f2 = Branch (env, Or (f1, f2), expand env f1, expand env f2)

(** [expand_implies f1 f2] expands [f1 -> f2] into a tableau. *)
and expand_implies env f1 f2 =
  Branch (env, Implies (f1, f2), expand env (Not f1), expand env f2)

(** [expand_iff f1 f2] expands [f1 <-> f2] into a tableau. *)
and expand_iff env f1 f2 =
  Branch
    ( env,
      Iff (f1, f2),
      expand env (And (f1, f2)),
      expand env (And (Not f1, Not f2)) )

(** [expand_forall x f] expands [forall x. f] into a tableau. 
    Note: expansion continues until either the max number of constants is reached or the tableau is closed. *)
and expand_forall (env, i) var f = 
  if i >= max_constants then
    Open (* TODO: handle this better *)
  else
    let f_subst = subst_formula var (Var ("#" ^ string_of_int i)) f in
    let expanded = expand (env, i + 1) f_subst in
    join expanded (Forall (var, f))

(** [expand_exists x f] expands [exists x. f] into a tableau.
    Introduces a new constant and substitutes it for [x]. *)
and expand_exists (env, i) var f = 
  let f_subst = subst_formula var (Var ("#" ^ string_of_int i)) f in
  expand (env, i + 1) f_subst

(** [join t1 t2] will extend all [Open] leaves of [t1] with [t2]. *)
and join t f =
  match t with
  | Branch (env, f', Open, Open) -> Branch (env, f', expand env f, Closed env)
  | Branch (env, f', Closed env', r1) -> Branch (env, f', Closed env', join r1 f)
  | Branch (env, f', l1, Closed env') -> Branch (env, f', join l1 f, Closed env')
  | Branch (env, f', l1, r1) -> Branch (env, f', join l1 f, join r1 f)
  | Closed env -> Closed env
  | Open -> failwith "unexpected Open"

(* TODO: substitution could be better if made lazy rather than eager, 
   this requires a rather large refactor since I'll need to also carry 
   a substitution map around inside env *)
(** [subst_formula x t f] substitutes [x] with [t] in [f]. *)
and subst_formula x t = function
  | Predicate (name, args) -> Predicate (name, List.map (subst_term x t) args)
  | Not f -> Not (subst_formula x t f)
  | And (f1, f2) -> And (subst_formula x t f1, subst_formula x t f2)
  | Or (f1, f2) -> Or (subst_formula x t f1, subst_formula x t f2)
  | Implies (f1, f2) -> Implies (subst_formula x t f1, subst_formula x t f2)
  | Iff (f1, f2) -> Iff (subst_formula x t f1, subst_formula x t f2)
  | Forall (y, f) -> if x = y then Forall (y, f) else Forall (y, subst_formula x t f)
  | Exists (y, f) -> if x = y then Exists (y, f) else Exists (y, subst_formula x t f)

(** [subst_term x t t'] substitutes [x] with [t] in [t']. *)
and subst_term x t = function
  | Var y -> if x = y then t else Var y
  | Fun (name, args) -> Fun (name, List.map (subst_term x t) args)

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