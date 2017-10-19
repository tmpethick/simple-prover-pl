
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

- Unsound: It parses invalid Isabelle. (is [b \equiv p] valid?)
- In-complete: It cannot parse all valid Isabelle. (left out a lot of operators)
- Structure and Printing: Precedence rules are not exactly right (if-else statements have higher precedence than some binary operators for which it should not)
- It parses more Isabelle than the prolog parser can handle.
- The prolog parser expect a structure with one top level equation an only one function application on the left side with only atoms e.g. `f a (b # c) \equiv ...`.
