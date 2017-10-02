


add(N, 0, N).
add(N, suc(M), suc(Y)) :- add(N, M, Y).

dec(0, 0).
dec(suc(N), N).

% TODO: how do we compose? U := dec (sub N M)
sub(N, 0, N).
sub(N, suc(M), U) :- sub(N, M, Y), dec(Y, U).

% TODO: how to do `map snd` in prolog
base([], []).
base([pair(_, NNF)|T], [NNF|CT]) :- base(T, CT).

dash(A, 0, A).
dash(A, suc(N), [N|A]).

% IDEA: Generalization: nested call e.g. `dash(dump t) h` => sequence of ANDs.
dump([], []).
dump([H|T], A) :- dump(T, S), dash(S, H, A).

% TODO: learn to trace debug
free(pre(_, _, V), V).
free(con(P, Q), V) :- free(P, F1), free(Q, F2), append(F1, F2, V).
free(dis(P, Q), V) :- free(P, F1), free(Q, F2), append(F1, F2, V).
free(uni(P), V) :- free(P, F), dump(F, V).
free(exi(P), V) :- free(P, F), dump(F, V).

over(S, _, 0, S).
over(_, H, suc(_), H).

% 1. Capitalize every argument
% 2. Replace `constants` like `ZeroNat`.
% 3. make `more x1 x2 = Y` into
%  | input is output    -> more(a1, a2, Y).
%  | input is func call -> more(a1, a2, Y) :- func(..., Y).
% 4. If func call then string together nested calls with `,`(AND).
% 5. Replace unused variables with `_`.
more(X, S, H, 0, Y) :- sub(X, H, XS), over(S, H, XS, Y).
more(_, _, H, suc(_), Y) :- dec(H, Y).

mend(_, _, [], []).
mend(X, S, [H|T], Y) :- sub(H, X, XS), mend(X, S, T, M), more(X, S, H, [XS|M], Y).

subst(X, S, pre(B, I, V), pre(B, I, W)) :-  mend(X, S, V, W).
subst(X, S, con(P, Q), con(PP, QQ)) :- subst(X, S, P, PP), subst(X, S, Q, QQ).
subst(X, S, dis(P, Q), dis(PP, QQ)) :-  subst(X, S, P, PP), subst(X, S, Q, QQ).
subst(X, S, uni(P), uni(PP)) :-  subst(suc(X), suc(S), P, PP).
subst(X, S, exi(P), exi(PP)) :-  subst(suc(X), suc(S), P, PP).

fresh([], 0).
fresh([H|T], suc(Y)) :- fresh(T, A), dec(A, B), sub(B, H, C), add(C, H, Y).

frees([], []).
frees([H|T], Y) :- free([H|frees(T)], Y).

stop(C, _, [], C).
stop(C, P, [H|T], Y) :- P = H -> Y = [] ; stop(C, P, T, Y).

% not?
track(S, _, pre(B, I, V), Y) :-
  append(S, [pair(0, pre(B, I, V))], SS),
  base(S, B),
  stop([SS], pre(not(B), I, V), B, Y).
% append case
track(S, _, con(P, Q), [SP, SQ]) :- 
  append(S, [pair(0, P)], SP),
  append(S, [pair(0, Q)], SQ).
track(S, _, dis(P, Q), [SP, SQ]) :- 
  append(S, [pair(0, P)], SP),
  append(S, [pair(0, Q)], SQ).
track(S, _, uni(P), Y) :- 
  base(S, B),
  frees([uni(P)|B], F),
  fresh(F, FF),
  subst(0, FF, P, SFF),
  append(S, [(0, SFF)], Y).
track(S, N, exi(P), Y) :- 
  subst(0, N, P, SP),
  append(S, [(0, SP), (suc(N), exi(P))], Y).

:- begin_tests(cases).
test(add) :- add(suc(suc(0)), suc(suc(suc(0))), suc(suc(suc(suc(suc(0)))))).
test(base) :- base([pair(1,2)], [2]).
test(dash0) :- dash([suc(0)], 0, [suc(0)]).
test(dash1) :- dash([suc(0)], suc(0), [0, suc(0)]).
test(dump) :- dump([0, suc(suc(0)), suc(0)], [suc(0), 0]).
test(free) :- 
  A = con(pre(1, 0, [suc(0)]), pre(1, 0, [suc(0)])),
  free(A,[suc(0), suc(0)]).
% over, mend, more, subst
test(fresh) :- fresh([suc(0), suc(suc(0)), 0], suc(suc(suc(0)))).
% frees
:- end_tests(cases).

% :-debug.
:- run_tests.
