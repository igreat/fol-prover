open Ast
open Utils
module Env = Set.Make (String)

type env = Env.t
type t = 
  | Branch of (env * int) * formula * t * t 
  | Closed of (env * int) * bool (* ((env, constant_cnt), is_contradiction) is kept for debugging *)
  | Open

let max_constants = 100

(** [negate f] converts [f] to [¬f], and [¬¬f] to [f]. *)
let negate = function
  | Not f -> f
  | f -> Not f

(** [add_to_env (env, i) s] adds [s] to [env] and returns the new environment. *)
let add_to_env (env, i) s = (Env.add s env, i)

(** [expand f] expands [f] into a tableau. *)
let rec expand env = function
  | Not f -> expand_not env f
  | And (f1, f2) -> expand_and env f1 f2
  | Or (f1, f2) -> expand_or env f1 f2
  | Implies (f1, f2) -> expand_implies env f1 f2
  | Iff (f1, f2) -> expand_iff env f1 f2
  | Forall (x, f) -> expand_forall env x f 0
  | Exists (x, f) -> expand_exists env x f
  | Predicate _ as pred ->
    let new_env = add_to_env env (string_of_formula pred) in
    if Env.mem (string_of_formula (negate pred)) (fst env) then
      Closed (new_env, true)
    else
      Branch (new_env, pred, Open, Open)

(** [expand_not f] expands [¬f] into a tableau. *)
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
      Closed (new_env, true)
    else
      Branch
        (new_env, negate pred, Open, Open)

(** [expand_and f1 f2] expands [f1 and f2] into a tableau. *)
and expand_and env f1 f2 =
  let t = match f1, f2 with
    | Forall _, _ 
    | _, Exists _ 
    | Not (Exists _), _ 
    | _, Not (Forall _) -> join_and (expand env f2) f1
    | _ -> join_and (expand env f1) f2
  in
  Branch (env, And (f1, f2), t, Closed (env, false))

(** [join_and t1 t2] will extend all [Open] leaves of [t1] with [t2]. *)
and join_and t f =
  match t with
  | Branch (env, f', Open, Open) -> 
    let expanded = expand env f in
    Branch (env, f', expanded, if expanded = Open then Open else Closed (env, false))
  | Branch (env, f', Closed (env', false), r1) -> Branch (env, f', Closed (env', false), join_and r1 f)
  | Branch (env, f', l1, Closed (env', false)) -> Branch (env, f', join_and l1 f, Closed (env', false))
  | Branch (env, f', l1, r1) -> Branch (env, f', join_and l1 f, join_and r1 f)
  | Closed _ as closed -> closed
  | Open -> failwith "unexpected Open in `and` expansion"

(** [expand_or f1 f2] expands [f1 or f2] into a tableau. *)
and expand_or env f1 f2 = 
  let left, right = expand env f1, expand env f2 in
  match left, right with
  | Open, Open -> Branch (env, Or (f1, f2), Open, Open)
  | Open, _ -> Branch (env, Or (f1, f2), Closed (env, false), right)
  | _, Open -> Branch (env, Or (f1, f2), left, Closed (env, false))
  | _, _ -> Branch (env, Or (f1, f2), left, right)

(** [expand_implies f1 f2] expands [f1 -> f2] into a tableau. *)
and expand_implies env f1 f2 =
  match expand_or env (Not f1) f2 with
  | Branch (env, _, l, r) -> Branch (env, Implies (f1, f2), l, r)
  | otherwise -> otherwise

(** [expand_iff f1 f2] expands [f1 <-> f2] into a tableau. *)
and expand_iff env f1 f2 =
  Branch
    ( env,
      Iff (f1, f2),
      expand env (And (f1, f2)),
      expand env (And (Not f1, Not f2)) )

(** [expand_forall x f cnt] expands [forall x. f] into a tableau.
    Note: expansion continues until either the max number of constants is reached or the tableau is closed. *)
and expand_forall (env, i) var f var_cnt = 
  if var_cnt >= max_constants then
    Open (* TODO: handle this better *)
  else
    let f_subst = subst_formula var (Var ("#" ^ string_of_int var_cnt)) f in
    let expanded = expand (env, max i (var_cnt + 1)) f_subst in
    join_forall expanded var f (var_cnt + 1)

(** [join_forall t var f var_cnt] will extend all [Open] leaves of [t] with [f] and [var] replaced with an existing constant. *)
and join_forall t var f var_cnt =
  match t with
  | Branch (env, f', Open, Open) -> 
    let expanded = expand_forall env var f var_cnt in
    Branch (env, f', expanded, if expanded = Open then Open else Closed (env, false))
  | Branch (env, f', Closed (env', _), r1) -> Branch (env, f', Closed (env', false), join_forall r1 var f var_cnt)
  | Branch (env, f', l1, Closed (env', _)) -> Branch (env, f', join_forall l1 var f var_cnt, Closed (env', false))
  | Branch (env, f', l1, r1) -> Branch (env, f', join_forall l1 var f var_cnt, join_forall r1 var f var_cnt)
  | Closed _ as closed -> closed
  | Open -> failwith "unexpected Open in `forall` expansion"

(** [expand_exists x f] expands [exists x. f] into a tableau.
    Introduces a new constant and substitutes it for [x]. *)
and expand_exists (env, i) var f = 
  if i >= max_constants then
    Open (* TODO: handle this better *)
  else
    let f_subst = subst_formula var (Var ("#" ^ string_of_int i)) f in
    expand (env, i + 1) f_subst

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
