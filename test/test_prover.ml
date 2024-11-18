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

(** [make_expand n s1 s2] makes an OUnit test named [n] that expects
    [s1] to expand to [s2]. *)
let make_expand n s1 s2 =
  n >:: fun _ -> assert_equal ~printer:string_of_tableau s2 (expand (Env.empty, 0) (parse s1))

(** [make_sat n s is_sat] makes an OUnit test named [n] that expects
    [s] to be satisfiable if [is_sat] is true, and unsatisfiable if
    [is_sat] is false. *)
let make_sat n s is_sat =
  n >:: fun _ -> assert_equal ~printer:string_of_bool is_sat (is_satisfiable (parse s))

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
    make_expand "expand predicate with no args" "P()" (Branch ((Env.singleton "P()", 0), Predicate ("P", []), Open, Open));
    make_expand "expand predicate with one arg" "P(x)" (Branch ((Env.singleton "P(x)", 0), Predicate ("P", [Var "x"]), Open, Open));
    make_expand "expand predicate with multiple args" "P(x, y)" (Branch ((Env.singleton "P(x, y)", 0), Predicate ("P", [Var "x"; Var "y"]), Open, Open));
    make_expand "expand not" "not P" (Branch ((Env.singleton "¬P()", 0), Not (Predicate ("P", [])), Open, Open));
    make_expand "expand and" "(P and Q)" 
      (
        Branch ((Env.empty, 0), And (Predicate ("P", []), Predicate ("Q", [])), 
                Branch ((Env.singleton "P()", 0), Predicate ("P", []), 
                        Branch ((Env.add "Q()" (Env.singleton "P()"), 0), Predicate ("Q", []), Open, Open), 
                        Closed (Env.singleton "P()", 0)),
                Closed (Env.empty , 0))
      );
    make_expand "expand or" "(P or Q)" 
      (
        Branch ((Env.empty, 0), Or (Predicate ("P", []), Predicate ("Q", [])), 
                Branch ((Env.singleton "P()", 0), Predicate ("P", []), Open, Open), 
                Branch ((Env.singleton "Q()", 0), Predicate ("Q", []), Open, Open))
      );
    make_expand "expand implies" "(P -> Q)" 
      (
        Branch ((Env.empty, 0), Implies (Predicate ("P", []), Predicate ("Q", [])), 
                Branch ((Env.singleton "¬P()", 0), Not (Predicate ("P", [])), Open, Open), 
                Branch ((Env.singleton "Q()", 0), Predicate ("Q", []), Open, Open))
      );
    make_expand "expand iff" "(P <-> Q)" 
      (
        Branch ((Env.empty, 0), Iff (Predicate ("P", []), Predicate ("Q", [])), 
                Branch ((Env.empty, 0), And (Predicate ("P", []), Predicate ("Q", [])), 
                        Branch ((Env.singleton "P()", 0), Predicate ("P", []), 
                                Branch ((Env.add "Q()" (Env.singleton "P()"), 0), Predicate ("Q", []), Open, Open), 
                                Closed (Env.singleton "P()", 0)),
                        Closed (Env.empty, 0)),
                Branch ((Env.empty, 0), And (Not (Predicate ("P", [])), Not (Predicate ("Q", []))),
                        Branch ((Env.singleton "¬P()", 0), Not (Predicate ("P", [])), 
                                Branch ((Env.add "¬Q()" (Env.singleton "¬P()"), 0), Not (Predicate ("Q", [])), Open, Open), 
                                Closed (Env.singleton "¬P()", 0)),
                        Closed (Env.empty, 0))
               )
      );
    make_expand "expand exists 1 var" "exists x (P(x))" (Branch ((Env.singleton "P(#0)", 1), Predicate ("P", [Var "#0"]), Open, Open));
    make_expand "expand exists 2 vars" "exists x (exists y (P(x, y)))" (Branch ((Env.singleton "P(#0, #1)", 2), Predicate ("P", [Var "#0"; Var "#1"]), Open, Open));
    make_expand "expand exists 2 vars with outter dependency" "exists x ((P(x) and exists y (P(x, y))))" 
      (
        Branch ((Env.empty, 1), And (Predicate ("P", [Var "#0"]), Exists ("y", Predicate ("P", [Var "#0"; Var "y"]))), 
                Branch ((env_from_list ["P(#0)"], 1), Predicate ("P", [Var "#0"]), 
                        Branch ((env_from_list ["P(#0)"; "P(#0, #1)"], 2), Predicate ("P", [Var "#0"; Var "#1"]), 
                                Open,
                                Open),
                        Closed (env_from_list ["P(#0)"], 1)),
                Closed (Env.empty, 1))
      );
    make_expand "expand exists 2 vars with lexical scoping" "exists x ((P(x) and exists x (P(x))))" 
      (
        Branch ((Env.empty, 1), And (Predicate ("P", [Var "#0"]), Exists ("x", Predicate ("P", [Var "x"]))), 
                Branch ((env_from_list ["P(#0)"], 1), Predicate ("P", [Var "#0"]), 
                        Branch ((env_from_list ["P(#0)"; "P(#1)"], 2), Predicate ("P", [Var "#1"]), 
                                Open, 
                                Open),
                        Closed (env_from_list ["P(#0)"], 1)),
                Closed (Env.empty, 1))
      );
    make_expand "formula closes" "(P and not P)" 
      (
        Branch ((Env.empty, 0), And (Predicate ("P", []), Not (Predicate ("P", []))), 
                Branch ((env_from_list ["P()"], 0), Predicate ("P", []), 
                        Closed (env_from_list ["P()"; "¬P()"], 0),
                        Closed (env_from_list ["P()"], 0)),
                Closed (Env.empty, 0))
      );
  ]
let satisfiability_tests = 
  [
    (* Implications *)
    make_sat "satisfiable implication" "(P -> Q)" true;
    make_sat "unsatisfiable implication with premises" "(((P -> Q) and P) and not Q)" false;

    (* Biconditionals *)
    make_sat "satisfiable biconditional" "(P <-> Q)" true;
    make_sat "unsatisfiable biconditional with contradiction" "(P <-> not P)" false;
    
    (* Nested Quantifiers *)
    make_sat "satisfiable nested quantifiers (forall exists)" "forall x (exists y (P(x, y)))" true;
    make_sat "unsatisfiable nested quantifiers (forall exists with contradiction)" "forall x (exists y ((P(x, y) and not P(x, y))))" false;
    
    (* Mixed Quantifiers *)
    make_sat "satisfiable mixed quantifiers (exists forall)" "exists x (forall y (P(x, y)))" true;
    make_sat "unsatisfiable mixed quantifiers (exists forall with contradiction)" "exists x (forall y ((P(x, y) and not P(x, y))))" false;
    
    (* Disjunctions *)
    make_sat "satisfiable disjunction" "(P or Q)" true;
    make_sat "unsatisfiable disjunction with contradiction" "(((P or Q) and not P) and not Q)" false;

    (* Conjunctions *)
    make_sat "satisfiable conjunction" "(P and Q)" true;
    make_sat "unsatisfiable conjunction with contradiction" "((P and Q) and not P)" false;

    (* Biconditionals with Multiple Predicates *)
    make_sat "satisfiable biconditional with multiple predicates" "((P <-> Q) and (R or S))" true;
    make_sat "unsatisfiable biconditional with multiple predicates and contradiction" "(((P <-> Q) and P) and not Q)" false;
    
    (* More Complex Nested Structures *)
    make_sat "satisfiable formula with multiple quantifiers and implications" "forall x ((P(x) -> exists y ((Q(y) and not Q(y)))))" true;
    make_sat "unsatisfiable formula with multiple quantifiers and implications" "forall x (forall y (((P(x) -> Q(y)) and (not Q(y) and P(x)))))" false;
  ]



let _ = run_test_tt_main ("suite" >::: parse_tests @ expand_tests @ satisfiability_tests)
