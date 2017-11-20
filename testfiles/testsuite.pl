:- begin_tests(cases).
:- ensure_loaded("prover-generated.pl").

test(add)   :- add(suc(suc(0)), suc(suc(suc(0))), suc(suc(suc(suc(suc(0)))))).
test(base)  :- base([(1,2)], [2]).
test(dash0) :- dash([suc(0)], 0, [suc(0)]).
test(dash1) :- dash([suc(0)], suc(0), [0, suc(0)]).
test(dump)  :- dump([0, suc(suc(0)), suc(0)], [suc(0), 0]).
test(free)  :- 
  A = con(pre(1, 0, [suc(0)]), pre(1, 0, [suc(0)])),
  free(A,[suc(0), suc(0)]).
test(fresh) :- fresh([suc(0), suc(suc(0)), 0], suc(suc(suc(0)))).
% TODO: test
% over, mend, more, subst, frees, stop, track, solve, solves, prover
test(track_dis) :-
  T = dis(pre(0, 0, [0]), pre(1, 0, [0])), 
  track([], 0, T, Y),
  Y = [[
    (0, pre(0, 0, [0])), 
    (0, pre(1, 0, [0]))
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
