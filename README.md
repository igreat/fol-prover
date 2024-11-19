# tiny prover

This is a tiny theorem prover that will determine the satisfiability or validity of a given first order logic formula. It's based on Tableaux method with a lot of implementation details designed by me.

```haskell
(Q -> P) and 
    (forall x not P(x)) and 
        (exists x P(x))
```

```haskell
Unsatisfiable
Tableau
└── (((Q() ⇒ P()) ∧ ∀x.(¬P(x))) ∧ ∃x.(P(x)))
    └── P(#0)
        └── ((Q() ⇒ P()) ∧ ∀x.(¬P(x)))
            └── (Q() ⇒ P())
                ├── ¬Q()
                │   └── ⊥ [1]{P(#0), ¬P(#0), ¬Q()}
                └── P()
                    └── ⊥ [1]{P(#0), P(), ¬P(#0)}
```

## How to use

First you need to ensure you have [dune](https://dune.build/) installed. Once you do, run the following commands to build the project:

```bash
dune build
dune install
```

After that, you can easily run the prover by running the following command:

```bash
prover --satisfiable --valid --tableau <path_to_file>
```

The `--satisfiable` and `valid` flags are to specify if you want to check for satisfiability and/or validity. Optionally, you can use the `--tableau` flag to pretty print the tableau tree. The `<path_to_file>` is the relative path to the file containing the formula you want to check. The formula should be in the correct syntax as described in [the syntax section](#syntax).

## Example

Here's an example of a formula that can be checked by the prover:

```haskell
(Q -> P) and 
    (forall x not P(x)) and 
        (exists x P(x))
```

This is an unsatisfiable formula (think about why!). If you run the following command:

```bash
prover --satisfiable --tableau examples/example1.txt
```

The output will be:

```haskell
Unsatisfiable
Tableau
└── (((Q() ⇒ P()) ∧ ∀x.(¬P(x))) ∧ ∃x.(P(x)))
    └── P(#0)
        └── ((Q() ⇒ P()) ∧ ∀x.(¬P(x)))
            └── (Q() ⇒ P())
                ├── ¬Q()
                │   └── ⊥ [1]{P(#0), ¬P(#0), ¬Q()}
                └── P()
                    └── ⊥ [1]{P(#0), P(), ¬P(#0)}
```

Notice that `⊥` signifies a closed branch. If all branches are closed, then the formula is unsatisfiable. If there's an open branch, then the formula is satisfiable.

## Syntax

First, there's the terms, which are defined as follows:

<!-- type term = Var of string | Fun of string * term list -->
- `x/y/z/my_var...`: a variable. Can consist of more than one character.
- `function(x, y, z, ...)` a function with a list of terms (can be variables or results of functions). The function name can consist of more than one character.

The connectives for the formulas is as follows in order of precedence (from highest to lowest):

- `not`: the logical negation operator
- `and`: the logical conjunction operator
- `or`: the logical disjunction operator
- `->`: the logical implication operator
- `<->`: the logical biconditional operator
- `forall x`: the universal quantifier for any variable `x`
- `exists x`: the existential quantifier for any variable `x`
- `Predicate(...terms)`: a predicate with a list of terms
- `( f )`: parentheses to force order of operations

Note: predicates MUST start with a capital letter, my recommendation is to use **PascalCase** for them. Similarly, variables and functions should start with a lowercase letter, my recommendation is to use **snake_case** for them.

## Limitations

As of right now this theorem prover able to deal with basically any first order logic formula that doesn't contain free variables. **One significant limitation (to be fixed soon)** is that when it reaches it's maximum "forall expansions", it will just say "satisfiable". I need to make it say "I don't know" instead for that branch.
