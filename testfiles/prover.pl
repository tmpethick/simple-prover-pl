
add(N, 0, N).
add(N, suc(M), suc(Y)) :- add(N, M, Y).

dec(0, 0).
dec(suc(N), N).

sub(N, 0, N).
sub(N, suc(M), U) :- sub(N, M, Y), dec(Y, U).

base([], []).
base([pair(_, NNF)|T], [NNF|CT]) :- base(T, CT).

dash(A, 0, A).
dash(A, suc(N), [N|A]).

dump([], []).
dump([H|T], A) :- dump(T, S), dash(S, H, A).

free(pre(_, _, V), V).
free(con(P, Q), V) :- free(P, F1), free(Q, F2), append(F1, F2, V).
free(dis(P, Q), V) :- free(P, F1), free(Q, F2), append(F1, F2, V).
free(uni(P), V) :- free(P, F), dump(F, V).
free(exi(P), V) :- free(P, F), dump(F, V).

over(S, _, 0, S).
over(_, H, suc(_), H).

more(X, S, H, 0, Y) :- sub(X, H, XS), over(S, H, XS, Y).
more(_, _, H, suc(_), Y) :- dec(H, Y).

mend(_, _, [], []).
mend(X, S, [H|T], [MO|ME]) :- sub(H, X, XS), more(X, S, H, XS, MO), mend(X, S, T, ME).

subst(X, S, pre(B, I, V), pre(B, I, W)) :-  mend(X, S, V, W).
subst(X, S, con(P, Q), con(PP, QQ)) :- subst(X, S, P, PP), subst(X, S, Q, QQ).
subst(X, S, dis(P, Q), dis(PP, QQ)) :-  subst(X, S, P, PP), subst(X, S, Q, QQ).
subst(X, S, uni(P), uni(PP)) :-  subst(suc(X), suc(S), P, PP).
subst(X, S, exi(P), exi(PP)) :-  subst(suc(X), suc(S), P, PP).

fresh([], 0).
fresh([H|T], suc(Y)) :- fresh(T, A), dec(A, B), sub(B, H, C), add(C, H, Y).

frees([], []).
frees([H|T], Y) :-  free(H, F), frees(T, FF), append(F, FF, Y).

stop(C, _, [], C).
stop(C, P, [H|T], Y) :- P = H -> Y = [] ; stop(C, P, T, Y).

negate(0, 1).
negate(1, 0).

track(S, _, pre(B, I, V), Y) :-
  append(S, [pair(0, pre(B, I, V))], SS),
  base(S, BASE),
  negate(B, NB),
  stop([SS], pre(NB, I, V), BASE, Y).
track(S, _, con(P, Q), [SP, SQ]) :- 
  append(S, [pair(0, P)], SP),
  append(S, [pair(0, Q)], SQ).
track(S, _, dis(P, Q), [SS]) :-
  append(S, [pair(0, P), pair(0, Q)], SS).
track(S, _, uni(P), [Y]) :- 
  base(S, B),
  frees([uni(P)|B], F),
  fresh(F, FF),
  subst(0, FF, P, SFF),
  append(S, [pair(0, SFF)], Y).
track(S, N, exi(P), [Y]) :- 
  subst(0, N, P, SP),
  append(S, [pair(0, SP), pair(suc(N), exi(P))], Y).

solve([], [[]]).
solve([pair(F, S)|T], Y) :- track(T, F, S, Y).

solves([], []).
solves([H|T], Y) :- solve(H, HS), solves(T, TS), append(HS, TS, Y).

prover([], 1).
prover([H|T], Y) :- solves([H|T], S), prover(S, Y).

check(P, Y) :- prover([[pair(0, P)]], Y).

:- begin_tests(cases).
test(add) :- add(suc(suc(0)), suc(suc(suc(0))), suc(suc(suc(suc(suc(0)))))).
test(base) :- base([pair(1,2)], [2]).
test(dash0) :- dash([suc(0)], 0, [suc(0)]).
test(dash1) :- dash([suc(0)], suc(0), [0, suc(0)]).
test(dump) :- dump([0, suc(suc(0)), suc(0)], [suc(0), 0]).
test(free) :- 
  A = con(pre(1, 0, [suc(0)]), pre(1, 0, [suc(0)])),
  free(A,[suc(0), suc(0)]).
test(fresh) :- fresh([suc(0), suc(suc(0)), 0], suc(suc(suc(0)))).
% TODO: test
% over, mend, more, subst, frees, stop, track, solve, solves, prover
test(track_dis) :-
  T = dis(pre(0, 0, [0]), pre(1, 0, [0])), 
  track([], 0, T, Y),
  Y = [[
    pair(0, pre(0, 0, [0])), 
    pair(0, pre(1, 0, [0]))
  ]].
test(check_simple) :-
  T = dis(
    pre(0, 0, [0]),
    pre(1, 0, [0])
  ),
  check(T, 1).
test(check) :-
  T =
    dis(
      uni(con(
        pre(0, 0, [0]),
        pre(0, suc(0), [0])
      )),
      dis(
        exi(pre(1, suc(0), [0])),
        exi(pre(1, 0, [0]))
      )
    ),
  check(T, 1).
:- end_tests(cases).

% :-trace(track).
% :-trace.
% :-debug.
:- run_tests.
