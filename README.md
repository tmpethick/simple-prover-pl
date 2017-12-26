
Simple Prover Compiler
======================

This project compiles a theorem prover written and proven with Isabelle and compiles it into Prolog.
It does so in Haskell through several catamorphism that changes the Isabelle AST into a Prolog AST.
It includes a Isabelle pretty printer only for testing the parser by exploiting that `prettyPrint . parse = id`.

It has three (brilliant) libraries as main dependencies:

- *Parsec* for parsing Isabelle.
- *Recursive Schemes* for generalized recursion methods like catamorphism and paramorphism.
- *PrettyPrint* by Daan Leijen based on Philip Wadler's prettier printer.


Tests
-----

To run Haskell tests:

```
stack test --file-watch
```

To generate and test the compiled prolog prover run:

```
make testprolog
```


General (manual) conversion approach
====================================


1. Capitalize every argument
2. Replace `constants` like `ZeroNat`.
3. make `more x1 x2 = y` into<br>
 | input is output    -> `more(A1, A2, Y).`<br>
 | input is func call -> `more(A1, A2, Y) :- func(..., Y).`
4. If composition of functions then string together nested calls with `,`(AND).
5. Replace unused variables with `_`.


Notes
-----

https://wiki.haskell.org/Parsing_expressions_and_statements
