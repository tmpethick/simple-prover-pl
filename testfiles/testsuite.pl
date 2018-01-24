:- begin_tests(cases).
:- ensure_loaded("prover-generated.pl").

% Definitions for writing formulas with infix operators.
:- op(650, xfy, con).
:- op(640, xfy, dis).
:- op(630, xfy, if).
:- op(630, xfy, imp).
:- op(620, xfy, iff).

% `not` is a better name but used by Simple Prover.
flip(0, 1).
flip(1, 0).

% Transform into Negation Normal Form.
nnf_conn(neg(P), neg(Y))          :- !, nnf_conn(P, Y).
nnf_conn(P dis Q, Y1 dis Y2)      :- !, nnf_conn(P, Y1), nnf_conn(Q, Y2).
nnf_conn(P con Q, Y1 con Y2)      :- !, nnf_conn(P, Y1), nnf_conn(Q, Y2).
nnf_conn(A iff B, Y)              :- !, nnf_conn((A imp B) con (B imp A), Y).
nnf_conn(P imp Q, neg(Y1) dis Y2) :- !, nnf_conn(P, Y1), nnf_conn(Q, Y2).
nnf_conn(P if  Q, Y1 dis neg(Y2)) :- !, nnf_conn(P, Y1), nnf_conn(Q, Y2).
nnf_conn(uni(P), uni(Y))          :- !, nnf_conn(P, Y).
nnf_conn(exi(P), exi(Y))          :- !, nnf_conn(P, Y).
nnf_conn(P, P).

nnf_push_neg(neg(pre(B,T,V)), pre(NB,T,V)) :- !, flip(B, NB).
nnf_push_neg(neg(neg(P)), Y)               :- !, nnf_push_neg(P, Y).
nnf_push_neg(neg(P dis Q), Y1 con Y2)      :- !, nnf_push_neg(neg(P), Y1), nnf_push_neg(neg(Q), Y2).
nnf_push_neg(neg(P con Q), Y1 dis Y2)      :- !, nnf_push_neg(neg(P), Y1), nnf_push_neg(neg(Q), Y2).
nnf_push_neg(neg(uni(P)), exi(Y))          :- !, nnf_push_neg(neg(P), Y).
nnf_push_neg(neg(exi(P)), uni(Y))          :- !, nnf_push_neg(neg(P), Y).
nnf_push_neg(P dis Q, Y1 dis Y2)           :- !, nnf_push_neg(P, Y1), nnf_push_neg(Q, Y2).
nnf_push_neg(P con Q, Y1 con Y2)           :- !, nnf_push_neg(P, Y1), nnf_push_neg(Q, Y2).
nnf_push_neg(uni(P), uni(Y))               :- !, nnf_push_neg(P, Y).
nnf_push_neg(exi(P), exi(Y))               :- !, nnf_push_neg(P, Y).
nnf_push_neg(P, P).

nnf(P, Y) :- nnf_conn(P, X), nnf_push_neg(X, Y).

nnf_then_check(T) :- nnf(T, T_), check(T_, 1).

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
  T = dis(
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
test(t01) :- nnf_then_check((pre(0, 0, []) imp pre(0, 0, []))).
test(t02) :- nnf_then_check((pre(0, 0, []) iff pre(0, 0, [])) dis pre(0, suc(0), [])).
test(t03) :- nnf_then_check((uni(pre(0, 0, [0, 0]))) imp (uni(exi(pre(0, 0, [0, suc(0)]))))).
test(t04) :- nnf_then_check((uni(exi(pre(0, 0, [0, suc(0)])))) if (uni(pre(0, 0, [0, 0])))).
test(t05) :- nnf_then_check((pre(0, suc(0), []) imp pre(0, suc(suc(0)), [])) imp ((pre(1, suc(0), []) imp pre(1, 0, [])) imp (pre(0, 0, []) imp pre(0, suc(suc(0)), [])))).
test(t06) :- nnf_then_check(((pre(0, 0, []) con pre(0, suc(0), [])) imp pre(0, suc(suc(0)), [])) imp (pre(0, 0, []) imp (pre(0, suc(0), []) imp pre(0, suc(suc(0)), [])))).
test(t07) :- nnf_then_check(pre(1, 0, []) imp pre(1, 0, [])).
test(t08) :- nnf_then_check((pre(0, 0, []) imp (pre(0, suc(0), []) imp pre(0, suc(suc(0)), []))) imp ((pre(0, 0, []) con pre(0, suc(0), [])) imp pre(0, suc(suc(0)), []))).
test(t09) :- nnf_then_check((pre(0, 0, []) imp pre(0, suc(0), [])) imp ((pre(0, 0, []) con pre(0, suc(suc(0)), [])) imp (pre(0, suc(0), []) con pre(0, suc(suc(0)), [])))).
test(t10) :- nnf_then_check((pre(0, 0, []) dis pre(0, suc(0), [])) imp (pre(0, suc(0), []) dis pre(0, 0, []))).
test(t11) :- nnf_then_check((pre(0, suc(0), []) imp pre(0, suc(suc(0)), [])) imp ((pre(0, 0, []) dis pre(0, suc(0), [])) imp (pre(0, 0, []) dis pre(0, suc(suc(0)), [])))).
test(t12) :- nnf_then_check(((pre(0, 0, []) dis pre(0, suc(0), [])) dis pre(0, suc(suc(0)), [])) imp (pre(0, 0, []) dis (pre(0, suc(0), []) dis pre(0, suc(suc(0)), [])))).
test(t13) :- nnf_then_check(pre(0, 0, []) imp (pre(0, suc(0), []) imp pre(0, 0, []))).
test(t14) :- nnf_then_check((pre(1, 0, []) dis pre(0, suc(0), [])) imp (pre(0, 0, []) imp pre(0, suc(0), []))).
test(t15) :- nnf_then_check(((pre(0, 0, []) imp pre(0, suc(0), [])) con (pre(0, 0, []) imp pre(1, suc(0), []))) imp (pre(1, 0, []))).
test(t16) :- nnf_then_check((pre(0, 0, []) imp pre(1, 0, [])) imp pre(1, 0, [])).
test(t17) :- nnf_then_check(((pre(0, 0, []) imp (pre(0, suc(0), []) imp pre(0, suc(suc(0)), []))) con (pre(0, 0, []) con pre(1, suc(suc(0)), []))) imp (pre(1, suc(0), []))).
test(t18) :- nnf_then_check((((pre(0, 0, []) con pre(1, suc(0), [])) imp pre(0, suc(suc(0)), [])) con ((pre(1, suc(suc(0)), [])) con pre(0, 0, []))) imp pre(0, suc(0), [])).
test(t19) :- nnf_then_check(((pre(0, 0, []) imp pre(0, suc(0), [])) con (pre(1, suc(0), []))) imp pre(1, 0, [])).
test(t20) :- nnf_then_check(pre(0, 0, []) imp (pre(0, 0, []))).
test(t21) :- nnf_then_check(pre(0, 0, []) dis pre(1, 0, [])).
test(t22) :- nnf_then_check((pre(0, 0, []) imp pre(0, suc(0), [])) imp (pre(1, 0, []) dis pre(0, suc(0), []))).
test(t23) :- nnf_then_check((((pre(0, 0, []) con pre(0, suc(0), [])) imp pre(0, suc(suc(0)), [])) con pre(0, 0, [])) imp (pre(0, suc(0), []) imp pre(0, suc(suc(0)), []))).
test(t24) :- nnf_then_check(((uni(pre(0, 0, [0]) imp pre(0, suc(0), [0]))) con (uni(pre(0, 0, [0])))) imp (uni(pre(0, suc(0), [0])))).
test(t25) :- nnf_then_check((pre(0, 0, [0]) con (uni(pre(0, 0, [0]) imp pre(1, suc(0), [0])))) imp (pre(1, suc(0), [0]))).
test(t26) :- nnf_then_check(uni(pre(0, 0, [0])) imp exi(pre(0, 0, [0]))).
test(t27) :- nnf_then_check(((uni(pre(0, 0, [0]) imp pre(0, suc(0), [0]))) con (exi(pre(0, 0, [0])))) imp (exi(pre(0, suc(0), [0])))).
test(t28) :- nnf_then_check(((uni(pre(0, suc(0), [0]) imp pre(0, suc(suc(0)), [0]))) con (exi(pre(0, 0, [0]) con pre(0, suc(0), [0])))) imp (exi(pre(0, 0, [0]) con pre(0, suc(suc(0)), [0])))).
test(t29) :- nnf_then_check((exi(pre(0, 0, [0])) con uni(uni(pre(0, 0, [0]) imp pre(0, suc(0), [suc(0)])))) imp (uni(pre(0, suc(0), [0])))).
test(t30) :- nnf_then_check((pre(1, 0, []) con pre(0, suc(0), [])) imp (pre(1, 0, []) dis pre(1, suc(0), []))).
test(t31) :- nnf_then_check(pre(1, 0, [])).
:- end_tests(cases).

% :-trace(nnf_then_check).
% :-trace.
% :-debug.
:- run_tests.
:- halt.
