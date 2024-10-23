open Ast

type t = Branch of formula * t * t | Leaf

(** [expand f] expands [f] into a tableau. *)
let rec expand = function
  | Not f -> expand_not f
  | And (f1, f2) -> expand_and f1 f2
  | Or (f1, f2) -> expand_or f1 f2
  | Implies (f1, f2) -> expand_implies f1 f2
  | Iff (f1, f2) -> expand_iff f1 f2
  | Forall (x, f) -> expand_forall x f
  | Exists (x, f) -> expand_exists x f
  | Predicate (p, ts) -> Branch (Predicate (p, ts), Leaf, Leaf)

and expand_not = function
  | Not f -> expand f
  | And (f1, f2) -> expand (Or (Not f1, Not f2))
  | Or (f1, f2) -> expand (And (Not f1, Not f2))
  | Implies (f1, f2) -> expand (And (f1, Not f2))
  | Iff (f1, f2) -> expand (Or (And (f1, Not f2), And (Not f1, f2)))
  | Forall (x, f) -> expand (Exists (x, Not f))
  | Exists (x, f) -> expand (Forall (x, Not f))
  | Predicate (p, ts) -> Branch (Not (Predicate (p, ts)), Leaf, Leaf)

and expand_and f1 f2 = Branch (And (f1, f2), join (expand f1) (expand f2), Leaf)
and expand_or f1 f2 = Branch (Or (f1, f2), expand f1, expand f2)
and expand_implies f1 f2 = Branch (Implies (f1, f2), expand (Not f1), expand f2)

and expand_iff f1 f2 =
  Branch (Iff (f1, f2), expand (And (f1, Not f2)), expand (And (Not f1, f2)))

and expand_forall _ _ = Leaf (* TODO *)
and expand_exists _ _ = Leaf (* TODO *)

(** [join t1 t2] will extend all leaves of [t1] with [t2]. *)
and join t1 t2 =
  match t1 with
  | Branch (f, Leaf, Leaf) -> Branch (f, t2, Leaf)
  | Branch (f, Leaf, r1) -> Branch (f, Leaf, join r1 t2)
  | Branch (f, l1, Leaf) -> Branch (f, join l1 t2, Leaf)
  | Branch (f, l1, r1) -> Branch (f, join l1 t2, join r1 t2)
  | Leaf -> failwith "unexpected Leaf"

