
General (manual) conversion approach
====================================

1. Capitalize every argument
2. Replace `constants` like `ZeroNat`.
3. make `more x1 x2 = y` into<br>
 | input is output    -> `more(A1, A2, Y).`<br>
 | input is func call -> `more(A1, A2, Y) :- func(..., Y).`
4. If composition of functions then string together nested calls with `,`(AND).
5. Replace unused variables with `_`.

Special case
------------

- Negating boolean is a built-in function in functional programming. 
Replace with user defined `negate`. 

Further ideas
-------------
- Generalize foldr (e.g. `solves`)
- Generalize `map snd` (e.g. `base`)
- Generalize nested call e.g. `dash(dump t) h` => sequence of ANDs (e.g. see `dump`). Is there any better way of composing?

Parser
------

- Unsound: It parses invalid Isabelle. (is `[b \<equiv> p]` valid?)
- In-complete: It cannot parse all valid Isabelle. (left out a lot of operators)
- Structure and Printing: Precedence rules are not exactly right (if-else statements have higher precedence than some binary operators for which it should not)
- It parses more Isabelle than the prolog parser can handle.
- The prolog parser expect a structure with one top level equation an only one function application on the left side with only atoms e.g. `f a (b # c) \equiv ...`.
- Added structure by introducing data types (capitalized functions like `Pre`). This is requires since these will be predicates in prolog with no "output".

TODO
----

- Quantifiers
- Multiple terms/statements
- Precedence for Prolog


Notes
-----

https://wiki.haskell.org/Parsing_expressions_and_statements

- Prolog decision: do not use true/false but instead built everything yourself (closer to the idea of Simple Prover and how not to rely on language specific features).
- Why not represent infix operators generally as `BinOp String a a`? Then we can't specify precedence.
TODO: bind together precedence on parsing and printing. Currently ifelse has incorrect precedence when parsed.

Token based passing (lexer)

Parsec
  - parenthesis are described in Parsec Not in data type/AST.
  - leave out all before `.`. Should be represented in to AST but not - used by prolog.
  - how to deal with False = True \equiv False (the infix function - definition)?

State: Functions are also TVal in functional languages. 
- Problem: how to capitalize only arguments and not function name?
- Solution: Use structure. Every SeqTerm with a starting TVal must - be a function. 
- Example: (draw tree for ((a b) c)).


Should 0, asd, "asd" and [] be represented differently in AST?
- Yes [] is necessary for changing representation.


Should @ be append on parsing?
- Should # be add


Tuples: parser will strip parentases. commas can then be parsed - as binary relations.
- Problem: collision with list type..


Point: testing can be done by defining pretty printing. (it is - the identity function then).

Examples:
  - prover (h # t) ≡ prover (solves (h # t))
  - "⋀p. check p ≡ prover [[(0,p)]]"
  - "⋀h t. prover (h # t) ≡ prover (solves (h # t))"
  - "prover [] ≡ True"
  - "solves [] ≡ []"
  - "⋀ h t. solves (h # t) ≡ solve h @ solves t"

Test cases
  - test equiv
  - test func app
  - test equiv
  - test precedence
  - test primitive
  - add support for [] (same as parens by what precedence?) [Exp] 
  - (supports [], [a#b], [a @ b] <-- just expressions inside. 
  -  tricky: [a,b], since a,b is tuple.)
  - add support for And
  - add tuple ,

how to deal with append when this is overwriten?
  - eq
  - quantifiers
