%{
open Ast
%}

%token FORALL
%token EXISTS
%token NOT
%token AND
%token OR
%token IMPLIES
%token IFF
%token LPAREN RPAREN
%token <string> TERM_ID
%token <string> PREDICATE_ID
%token EOF

%nonassoc FORALL EXISTS
%left IFF
%left IMPLIES
%left OR
%left AND
%right NOT

%start <Ast.formula> prog

%%

prog:
  | f = formula EOF { f }
  ;

formula:
  | NOT; f = formula { Not f }
  | FORALL; x = TERM_ID; f = formula { Forall (x, f) }
  | EXISTS; x = TERM_ID; f = formula { Exists (x, f) }
  | f1 = formula; AND; f2 = formula; { And (f1, f2) }
  | f1 = formula; OR; f2 = formula; { Or (f1, f2) }
  | f1 = formula; IMPLIES; f2 = formula; { Implies (f1, f2) }
  | f1 = formula; IFF; f2 = formula; { Iff (f1, f2) }
  | LPAREN f = formula RPAREN { f }
  | p = predicate { p }
  ;

predicate:
  | x = PREDICATE_ID; LPAREN; ts = term_list; RPAREN { Predicate (x, List.rev ts) }
  | x = PREDICATE_ID { Predicate (x, []) }
  ; 

term_list:
  | { [] }
  | t = term { [t] }
  | ts = term_list; t = term { t :: ts }
  ;

term:
  | x = TERM_ID LPAREN; ts = term_list; RPAREN { Fun (x, List.rev ts) }
  | x = TERM_ID { Var x }
  ;