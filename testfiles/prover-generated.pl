check(P,Y) :- prover([[(0,P)]],Y).

prover([H|T],Y) :- solves([H|T],X0), prover(X0,Y).

prover([],1).

solves([],[]).

solves([H|T],Y) :- solve(H,X0), solves(T,X1), append(X0,X1,Y).

solve([],[[]]).

solve([H|T],Y) :- fst(H,X0), snd(H,X1), track(T,X0,X1,Y).

track(S,N,pre(B,I,V),Y) :- append(S,[(0,pre(B,I,V))],X0), not(B,X1), base(S,X2), stop([X0],pre(X1,I,V),X2,Y).

track(S,N,con(P,Q),[X0,X1]) :- append(S,[(0,P)],X0), append(S,[(0,Q)],X1).

track(S,N,dis(P,Q),[X0]) :- append(S,[(0,P),(0,Q)],X0).

track(S,N,uni(P),[X4]) :- base(S,X0), frees([uni(P)|X0],X1), fresh(X1,X2), subst(0,X2,P,X3), append(S,[(0,X3)],X4).

track(S,N,exi(P),[X1]) :- subst(0,N,P,X0), append(S,[(0,X0),(suc(N),exi(P))],X1).

stop(C,P,[],C).

stop(C,P,[H|T],Y) :- eq(P,H,X0), stop(C,P,T,X1), ifelse(X0,[],X1,Y).

base([],[]).

base([H|T],[X0|X1]) :- snd(H,X0), base(T,X1).

subst(X,S,pre(B,I,V),pre(B,I,X0)) :- mend(X,S,V,X0).

subst(X,S,con(P,Q),con(X0,X1)) :- subst(X,S,P,X0), subst(X,S,Q,X1).

subst(X,S,dis(P,Q),dis(X0,X1)) :- subst(X,S,P,X0), subst(X,S,Q,X1).

subst(X,S,uni(P),uni(X0)) :- subst(suc(X),suc(S),P,X0).

subst(X,S,exi(P),exi(X0)) :- subst(suc(X),suc(S),P,X0).

mend(X,S,[],[]).

mend(X,S,[H|T],[X1|X2]) :- sub(H,X,X0), more(X,S,H,X0,X1), mend(X,S,T,X2).

more(X,S,H,0,Y) :- sub(X,H,X0), over(S,H,X0,Y).

more(X,S,H,suc(N),Y) :- dec(H,Y).

over(S,H,0,S).

over(S,H,suc(N),H).

fresh([],0).

fresh([H|T],suc(X3)) :- fresh(T,X0), dec(X0,X1), sub(X1,H,X2), add(X2,H,X3).

frees([],[]).

frees([H|T],Y) :- free(H,X0), frees(T,X1), append(X0,X1,Y).

free(pre(B,I,V),V).

free(con(P,Q),Y) :- free(P,X0), free(Q,X1), append(X0,X1,Y).

free(dis(P,Q),Y) :- free(P,X0), free(Q,X1), append(X0,X1,Y).

free(uni(P),Y) :- free(P,X0), dump(X0,Y).

free(exi(P),Y) :- free(P,X0), dump(X0,Y).

dump([],[]).

dump([H|T],Y) :- dump(T,X0), dash(X0,H,Y).

dash(L,0,L).

dash(L,suc(N),[N|L]).

eq(pre(B,I,V),con(P,Q),0).

eq(con(P,Q),pre(B,I,V),0).

eq(pre(B,I,V),dis(P,Q),0).

eq(dis(P,Q),pre(B,I,V),0).

eq(pre(B,I,V),uni(P),0).

eq(uni(P),pre(B,I,V),0).

eq(pre(B,I,V),exi(P),0).

eq(exi(P),pre(B,I,V),0).

eq(con(P,Q),dis(P_,Q_),0).

eq(dis(P_,Q_),con(P,Q),0).

eq(con(P,Q),uni(P_),0).

eq(uni(P_),con(P,Q),0).

eq(con(P,Q),exi(P_),0).

eq(exi(P_),con(P,Q),0).

eq(dis(P,Q),uni(P_),0).

eq(uni(P_),dis(P,Q),0).

eq(dis(P,Q),exi(P_),0).

eq(exi(P_),dis(P,Q),0).

eq(uni(P),exi(P_),0).

eq(exi(P_),uni(P),0).

eq(pre(B,I,V),pre(B_,I_,V_),Y) :- eq(B,B_,X0), eq(I,I_,X1), conj(X0,X1,X2), eq(V,V_,X3), conj(X2,X3,Y).

eq(con(P,Q),con(P_,Q_),Y) :- eq(P,P_,X0), eq(Q,Q_,X1), conj(X0,X1,Y).

eq(dis(P,Q),dis(P_,Q_),Y) :- eq(P,P_,X0), eq(Q,Q_,X1), conj(X0,X1,Y).

eq(uni(P),uni(P_),Y) :- eq(P,P_,Y).

eq(exi(P),exi(P_),Y) :- eq(P,P_,Y).

add(X,0,X).

add(X,suc(N),suc(X0)) :- add(X,N,X0).

sub(X,0,X).

sub(X,suc(N),Y) :- sub(X,N,X0), dec(X0,Y).

dec(0,0).

dec(suc(N),N).

append([],L,L).

append([H|T],L,[H|X0]) :- append(T,L,X0).

ifelse(1,X,Y,X).

ifelse(0,X,Y,Y).

not(1,0).

not(0,1).

fst((X,Y),X).

snd((X,Y),Y).

eq(0,0,1).

eq([],[],1).

eq(1,1,1).

eq(0,0,1).

conj(1,B,B).

conj(0,B,0).

eq(0,suc(N),0).

eq(suc(N),0,0).

eq([],[H|T],0).

eq([H|T],[],0).

eq(1,0,0).

eq(0,1,0).

eq((X,Y),(X_,Y_),Y) :- eq(X,X_,X0), eq(Y,Y_,X1), conj(X0,X1,Y).

eq(suc(N),suc(N_),Y) :- eq(N,N_,Y).

eq([H|T],[H_|T_],Y) :- eq(H,H_,X0), eq(T,T_,X1), conj(X0,X1,Y).