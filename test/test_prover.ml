open OUnit2
open Prover
open Ast
open Main
open Utils
open Tableau

(** [make n s1 s2] makes an OUnit test named [n] that expects
    [s1] to parse to [s2]. *)

let make_parse n s1 s2 =
  n >:: fun _ -> assert_equal ~printer:string_of_formula s2 (parse s1)

let make_expand n s1 s2 =
  n >:: fun _ -> assert_equal ~printer:string_of_tableau s2 (expand (Env.empty, 0) (parse s1))

let env_from_list l = List.fold_left (fun acc x -> Env.add x acc) Env.empty l
let parse_tests =
  [
    make_parse "parse predicate with no args v1" "P()" (Predicate ("P", []));
    make_parse "parse predicate with no args v2" "P" (Predicate ("P", []));
    make_parse "parse function with no args" "P(f())"
      (Predicate ("P", [ Fun ("f", []) ]));
    make_parse "parse function with one arg" "P(f(x))"
      (Predicate ("P", [ Fun ("f", [ Var "x" ]) ]));
    make_parse "parse function with multiple args" "P(f(x,y))"
      (Predicate ("P", [ Fun ("f", [ Var "x"; Var "y" ]) ]));
    make_parse "parse variable" "P(x)" (Predicate ("P", [ Var "x" ]));
    make_parse "parse variable" "P(x,y)" (Predicate ("P", [ Var "x"; Var "y" ]));
    make_parse "parse forall" "forall x (P(x))"
      (Forall ("x", Predicate ("P", [ Var "x" ])));
    make_parse "parse exists" "exists x (P(x))"
      (Exists ("x", Predicate ("P", [ Var "x" ])));
    make_parse "parse not v1" "not P" (Not (Predicate ("P", [])));
    make_parse "parse not v2" "(not (P(x) -> Q(y)) -> R(z))"
      (Implies
         ( Not
             (Implies
                (Predicate ("P", [ Var "x" ]), Predicate ("Q", [ Var "y" ]))),
           Predicate ("R", [ Var "z" ]) ));
    make_parse "parse and" "(P and Q)"
      (And (Predicate ("P", []), Predicate ("Q", [])));
    make_parse "parse or" "(P or Q)"
      (Or (Predicate ("P", []), Predicate ("Q", [])));
    make_parse "parse implies" "(P -> Q)"
      (Implies (Predicate ("P", []), Predicate ("Q", [])));
    make_parse "parse iff" "(P <-> Q)" (Iff (Predicate ("P", []), Predicate ("Q", [])));
  ]

let expand_tests = 
  [
    make_expand "expand predicate with no args" "P()" (Branch ((Env.singleton "P()", 0), Predicate ("P", []), Leaf, Leaf));
    make_expand "expand predicate with one arg" "P(x)" (Branch ((Env.singleton "P(x)", 0), Predicate ("P", [Var "x"]), Leaf, Leaf));
    make_expand "expand predicate with multiple args" "P(x, y)" (Branch ((Env.singleton "P(x, y)", 0), Predicate ("P", [Var "x"; Var "y"]), Leaf, Leaf));
    make_expand "expand not" "not P" (Branch ((Env.singleton "¬P()", 0), Not (Predicate ("P", [])), Leaf, Leaf));
    make_expand "expand and" "(P and Q)" 
      (
        Branch ((Env.empty, 0), And (Predicate ("P", []), Predicate ("Q", [])), 
                Branch ((Env.singleton "P()", 0), Predicate ("P", []), 
                        Branch ((Env.add "Q()" (Env.singleton "P()"), 0), Predicate ("Q", []), Leaf, Leaf), 
                        Leaf), 
                Leaf)
      );
    make_expand "expand or" "(P or Q)" 
      (
        Branch ((Env.empty, 0), Or (Predicate ("P", []), Predicate ("Q", [])), 
                Branch ((Env.singleton "P()", 0), Predicate ("P", []), Leaf, Leaf), 
                Branch ((Env.singleton "Q()", 0), Predicate ("Q", []), Leaf, Leaf))
      );
    make_expand "expand implies" "(P -> Q)" 
      (
        Branch ((Env.empty, 0), Implies (Predicate ("P", []), Predicate ("Q", [])), 
                Branch ((Env.singleton "¬P()", 0), Not (Predicate ("P", [])), Leaf, Leaf), 
                Branch ((Env.singleton "Q()", 0), Predicate ("Q", []), Leaf, Leaf))
      );
    make_expand "expand iff" "(P <-> Q)" 
      (
        Branch ((Env.empty, 0), Iff (Predicate ("P", []), Predicate ("Q", [])), 
                Branch ((Env.empty, 0), And (Predicate ("P", []), Predicate ("Q", [])), 
                        Branch ((Env.singleton "P()", 0), Predicate ("P", []), 
                                Branch ((Env.add "Q()" (Env.singleton "P()"), 0), Predicate ("Q", []), Leaf, Leaf), 
                                Leaf), 
                        Leaf),
                Branch ((Env.empty, 0), And (Not (Predicate ("P", [])), Not (Predicate ("Q", []))),
                        Branch ((Env.singleton "¬P()", 0), Not (Predicate ("P", [])), 
                                Branch ((Env.add "¬Q()" (Env.singleton "¬P()"), 0), Not (Predicate ("Q", [])), Leaf, Leaf), 
                                Leaf),
                        Leaf)
               )
      );
    make_expand "expand exists 1 var" "exists x (P(x))" (Branch ((Env.singleton "P(#0)", 1), Predicate ("P", [Var "#0"]), Leaf, Leaf));
    make_expand "expand exists 2 vars" "exists x (exists y (P(x, y)))" (Branch ((Env.singleton "P(#0, #1)", 2), Predicate ("P", [Var "#0"; Var "#1"]), Leaf, Leaf));
    make_expand "expand forall 1 var" "forall x (P(x))" 
      (
        Branch ((env_from_list ["P(#0)"], 1), Predicate ("P", [Var "#0"]), 
                Branch ((env_from_list ["P(#0)"; "P(#1)"], 2), Predicate ("P", [Var "#1"]),
                        Branch((env_from_list ["P(#0)"; "P(#1)"; "P(#2)"], 3), Predicate ("P", [Var "#2"]), 
                               Branch((env_from_list ["P(#0)"; "P(#1)"; "P(#2)"; "P(#3)"], 4), Predicate ("P", [Var "#3"]), 
                                      Branch((env_from_list ["P(#0)"; "P(#1)"; "P(#2)"; "P(#3)"; "P(#4)"], 5), Predicate ("P", [Var "#4"]), 
                                             Branch((env_from_list ["P(#0)"; "P(#1)"; "P(#2)"; "P(#3)"; "P(#4)"; "P(#5)"], 6), Predicate ("P", [Var "#5"]), 
                                                    Branch((env_from_list ["P(#0)"; "P(#1)"; "P(#2)"; "P(#3)"; "P(#4)"; "P(#5)"; "P(#6)"], 7), Predicate ("P", [Var "#6"]), 
                                                           Branch((env_from_list ["P(#0)"; "P(#1)"; "P(#2)"; "P(#3)"; "P(#4)"; "P(#5)"; "P(#6)"; "P(#7)"], 8), Predicate ("P", [Var "#7"]), 
                                                                  Branch((env_from_list ["P(#0)"; "P(#1)"; "P(#2)"; "P(#3)"; "P(#4)"; "P(#5)"; "P(#6)"; "P(#7)"; "P(#8)"], 9), Predicate ("P", [Var "#8"]), 
                                                                         Branch((env_from_list ["P(#0)"; "P(#1)"; "P(#2)"; "P(#3)"; "P(#4)"; "P(#5)"; "P(#6)"; "P(#7)"; "P(#8)"; "P(#9)"], 10), Predicate ("P", [Var "#9"]), 
                                                                                Leaf, Leaf), 
                                                                         Leaf), 
                                                                  Leaf), 
                                                           Leaf), 
                                                    Leaf), 
                                             Leaf), 
                                      Leaf), 
                               Leaf),
                        Leaf),
                Leaf)
      );
  ]
let _ = run_test_tt_main ("suite" >::: parse_tests @ expand_tests)
