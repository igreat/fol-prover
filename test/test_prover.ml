open OUnit2
open Prover
open Ast
open Main

(** [make n s1 s2] makes an OUnit test named [n] that expects
    [s1] to parse to [s2]. *)

let make_parse n s1 s2 =
  n >:: fun _ -> assert_equal ~printer:string_of_formula (parse s1) s2

let tests =
  [
    make_parse "predicate with no args v1" "P()" (Predicate ("P", []));
    make_parse "predicate with no args v2" "P" (Predicate ("P", []));
    make_parse "function with no args" "P(f())"
      (Predicate ("P", [ Fun ("f", []) ]));
    make_parse "function with one arg" "P(f(x))"
      (Predicate ("P", [ Fun ("f", [ Var "x" ]) ]));
    make_parse "function with multiple args" "P(f(x,y))"
      (Predicate ("P", [ Fun ("f", [ Var "x"; Var "y" ]) ]));
    make_parse "variable" "P(x)" (Predicate ("P", [ Var "x" ]));
    make_parse "variable" "P(x,y)" (Predicate ("P", [ Var "x"; Var "y" ]));
    make_parse "forall" "forall x (P(x))"
      (Forall ("x", Predicate ("P", [ Var "x" ])));
    make_parse "exists" "exists x (P(x))"
      (Exists ("x", Predicate ("P", [ Var "x" ])));
    make_parse "not v1" "not P" (Not (Predicate ("P", [])));
    make_parse "not v2" "(not (P(x) -> Q(y)) -> R(z))"
      (Implies
         ( Not
             (Implies
                (Predicate ("P", [ Var "x" ]), Predicate ("Q", [ Var "y" ]))),
           Predicate ("R", [ Var "z" ]) ));
    make_parse "and" "(P and Q)"
      (And (Predicate ("P", []), Predicate ("Q", [])));
    make_parse "or" "(P or Q)"
      (Or (Predicate ("P", []), Predicate ("Q", [])));
    make_parse "implies" "(P -> Q)"
      (Implies (Predicate ("P", []), Predicate ("Q", [])));
    make_parse "iff" "(P <-> Q)" (Iff (Predicate ("P", []), Predicate ("Q", [])));
  ]

let _ = run_test_tt_main ("suite" >::: tests)
