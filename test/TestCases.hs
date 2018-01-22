module TestCases where

simpleProver = fmap fst simpleProverPair
  

simpleProverPair = 
  [("\\<open>\\<And>p. check p \\<equiv> prover [[(0,p)]]\\<close>",
    "check(P,Y) :- prover([[(0,P)]],Y)."),
   ("\\<open>\\<And>h t. prover (h # t) \\<equiv> prover (solves (h # t))\\<close>",
    "prover([H|T],Y) :- solves([H|T],X0), prover(X0,Y)."),
   ("\\<open>prover [] \\<equiv> True\\<close>",
    "prover([],1)."),
   ("\\<open>solves [] \\<equiv> []\\<close>",
    "solves([],[])."),
   ("\\<open>\\<And>h t. solves (h # t) \\<equiv> solve h @ solves t\\<close>",
    "solves([H|T],Y) :- solve(H,X0), solves(T,X1), append(X0,X1,Y)."),
   ("\\<open>solve [] \\<equiv> [[]]\\<close>",
    "solve([],[[]])."),
   ("\\<open>\\<And>h t. solve (h # t) \\<equiv> track t (fst h) (snd h)\\<close>",
    "solve([H|T],Y) :- fst(H,X0), snd(H,X1), track(T,X0,X1,Y)."),
   ("\\<open>\\<And>s n b i v. track s n (Pre b i v) \\<equiv> stop [s @ [(0,Pre b i v)]] (Pre (\\<not> b) i v) (base s)\\<close>",
    "track(S,_,pre(B,I,V),Y) :- append(S,[(0,pre(B,I,V))],X0), not(B,X1), base(S,X2), stop([X0],pre(X1,I,V),X2,Y)."),
   ("\\<open>\\<And>s n p q. track s n (Con p q) \\<equiv> [s @ [(0,p)],s @ [(0,q)]]\\<close>",
    "track(S,_,con(P,Q),[X0,X1]) :- append(S,[(0,P)],X0), append(S,[(0,Q)],X1)."),
   ("\\<open>\\<And>s n p q. track s n (Dis p q) \\<equiv> [s @ [(0,p),(0,q)]]\\<close>",
    "track(S,_,dis(P,Q),[X0]) :- append(S,[(0,P),(0,Q)],X0)."),
   ("\\<open>\\<And>s n p. track s n (Uni p) \\<equiv> [s @ [(0,subst 0 (fresh (frees (Uni p # base s))) p)]]\\<close>",
    "track(S,_,uni(P),[X4]) :- base(S,X0), frees([uni(P)|X0],X1), fresh(X1,X2), subst(0,X2,P,X3), append(S,[(0,X3)],X4)."),
   ("\\<open>\\<And>s n p. track s n (Exi p) \\<equiv> [s @ [(0,subst 0 n p),(Suc n,Exi p)]]\\<close>",
    "track(S,N,exi(P),[X1]) :- subst(0,N,P,X0), append(S,[(0,X0),(suc(N),exi(P))],X1)."),
   ("\\<open>\\<And>c p. stop c p [] \\<equiv> c\\<close>",
    "stop(C,_,[],C)."),
   ("\\<open>\\<And>c p h t. stop c p (h # t) \\<equiv> (if p = h then [] else stop c p t)\\<close>",
    "stop(C,P,[H|T],Y) :- eq(P,H,X0), stop(C,P,T,X1), ifelse(X0,[],X1,Y)."),
   ("\\<open>base [] \\<equiv> []\\<close>",
    "base([],[])."),
   ("\\<open>\\<And>h t. base (h # t) \\<equiv> snd h # base t\\<close>",
    "base([H|T],[X0|X1]) :- snd(H,X0), base(T,X1)."),
   ("\\<open>\\<And>x s b i v. subst x s (Pre b i v) \\<equiv> Pre b i (mend x s v)\\<close>",
    "subst(X,S,pre(B,I,V),pre(B,I,X0)) :- mend(X,S,V,X0)."),
   ("\\<open>\\<And>x s p q. subst x s (Con p q) \\<equiv> Con (subst x s p) (subst x s q)\\<close>",
    "subst(X,S,con(P,Q),con(X0,X1)) :- subst(X,S,P,X0), subst(X,S,Q,X1)."),
   ("\\<open>\\<And>x s p q. subst x s (Dis p q) \\<equiv> Dis (subst x s p) (subst x s q)\\<close>",
    "subst(X,S,dis(P,Q),dis(X0,X1)) :- subst(X,S,P,X0), subst(X,S,Q,X1)."),
   ("\\<open>\\<And>x s p. subst x s (Uni p) \\<equiv> Uni (subst (Suc x) (Suc s) p)\\<close>",
    "subst(X,S,uni(P),uni(X0)) :- subst(suc(X),suc(S),P,X0)."),
   ("\\<open>\\<And>x s p. subst x s (Exi p) \\<equiv> Exi (subst (Suc x) (Suc s) p)\\<close>",
    "subst(X,S,exi(P),exi(X0)) :- subst(suc(X),suc(S),P,X0)."),
   ("\\<open>\\<And>x s. mend x s [] \\<equiv> []\\<close>",
    "mend(_,_,[],[])."),
   ("\\<open>\\<And>x s h t. mend x s (h # t) \\<equiv> more x s h (sub h x) # mend x s t\\<close>",
    "mend(X,S,[H|T],[X1|X2]) :- sub(H,X,X0), more(X,S,H,X0,X1), mend(X,S,T,X2)."),
   ("\\<open>\\<And>x s h. more x s h 0 \\<equiv> over s h (sub x h)\\<close>",
    "more(X,S,H,0,Y) :- sub(X,H,X0), over(S,H,X0,Y)."),
   ("\\<open>\\<And>x s h n. more x s h (Suc n) \\<equiv> dec h\\<close>",
    "more(_,_,H,suc(_),Y) :- dec(H,Y)."),
   ("\\<open>\\<And>s h. over s h 0 \\<equiv> s\\<close>",
    "over(S,_,0,S)."),
   ("\\<open>\\<And>s h n. over s h (Suc n) \\<equiv> h\\<close>",
    "over(_,H,suc(_),H)."),
   ("\\<open>fresh [] \\<equiv> 0\\<close>",
    "fresh([],0)."),
   ("\\<open>\\<And>h t. fresh (h # t) \\<equiv> Suc (add (sub (dec (fresh t)) h) h)\\<close>",
    "fresh([H|T],suc(X3)) :- fresh(T,X0), dec(X0,X1), sub(X1,H,X2), add(X2,H,X3)."),
   ("\\<open>frees [] \\<equiv> []\\<close>",
    "frees([],[])."),
   ("\\<open>\\<And>h t. frees (h # t) \\<equiv> free h @ frees t\\<close>",
    "frees([H|T],Y) :- free(H,X0), frees(T,X1), append(X0,X1,Y)."),
   ("\\<open>\\<And>b i v. free (Pre b i v) \\<equiv> v\\<close>",
    "free(pre(_,_,V),V)."),
   ("\\<open>\\<And>p q. free (Con p q) \\<equiv> free p @ free q\\<close>",
    "free(con(P,Q),Y) :- free(P,X0), free(Q,X1), append(X0,X1,Y)."),
   ("\\<open>\\<And>p q. free (Dis p q) \\<equiv> free p @ free q\\<close>",
    "free(dis(P,Q),Y) :- free(P,X0), free(Q,X1), append(X0,X1,Y)."),
   ("\\<open>\\<And>p. free (Uni p) \\<equiv> dump (free p)\\<close>",
    "free(uni(P),Y) :- free(P,X0), dump(X0,Y)."),
   ("\\<open>\\<And>p. free (Exi p) \\<equiv> dump (free p)\\<close>",
    "free(exi(P),Y) :- free(P,X0), dump(X0,Y)."),
   ("\\<open>dump [] \\<equiv> []\\<close>",
    "dump([],[])."),
   ("\\<open>\\<And>h t. dump (h # t) \\<equiv> dash (dump t) h\\<close>",
    "dump([H|T],Y) :- dump(T,X0), dash(X0,H,Y)."),
   ("\\<open>\\<And>l. dash l 0 \\<equiv> l\\<close>",
    "dash(L,0,L)."),
   ("\\<open>\\<And>l n. dash l (Suc n) \\<equiv> n # l\\<close>",
    "dash(L,suc(N),[N|L])."),
   ("\\<open>\\<And>b i v p q. Pre b i v = Con p q \\<equiv> False\\<close>",
    "eq(pre(_,_,_),con(_,_),0)."),
   ("\\<open>\\<And>b i v p q. Con p q = Pre b i v \\<equiv> False\\<close>",
    "eq(con(_,_),pre(_,_,_),0)."),
   ("\\<open>\\<And>b i v p q. Pre b i v = Dis p q \\<equiv> False\\<close>",
    "eq(pre(_,_,_),dis(_,_),0)."),
   ("\\<open>\\<And>b i v p q. Dis p q = Pre b i v \\<equiv> False\\<close>",
    "eq(dis(_,_),pre(_,_,_),0)."),
   ("\\<open>\\<And>b i v p. Pre b i v = Uni p \\<equiv> False\\<close>",
    "eq(pre(_,_,_),uni(_),0)."),
   ("\\<open>\\<And>b i v p. Uni p = Pre b i v \\<equiv> False\\<close>",
    "eq(uni(_),pre(_,_,_),0)."),
   ("\\<open>\\<And>b i v p. Pre b i v = Exi p \\<equiv> False\\<close>",
    "eq(pre(_,_,_),exi(_),0)."),
   ("\\<open>\\<And>b i v p. Exi p = Pre b i v \\<equiv> False\\<close>",
    "eq(exi(_),pre(_,_,_),0)."),
   ("\\<open>\\<And>p q p' q'. Con p q = Dis p' q' \\<equiv> False\\<close>",
    "eq(con(_,_),dis(_,_),0)."),
   ("\\<open>\\<And>p q p' q'. Dis p' q' = Con p q \\<equiv> False\\<close>",
    "eq(dis(_,_),con(_,_),0)."),
   ("\\<open>\\<And>p q p'. Con p q = Uni p' \\<equiv> False\\<close>",
    "eq(con(_,_),uni(_),0)."),
   ("\\<open>\\<And>p q p'. Uni p' = Con p q \\<equiv> False\\<close>",
    "eq(uni(_),con(_,_),0)."),
   ("\\<open>\\<And>p q p'. Con p q = Exi p' \\<equiv> False\\<close>",
    "eq(con(_,_),exi(_),0)."),
   ("\\<open>\\<And>p q p'. Exi p' = Con p q \\<equiv> False\\<close>",
    "eq(exi(_),con(_,_),0)."),
   ("\\<open>\\<And>p q p'. Dis p q = Uni p' \\<equiv> False\\<close>",
    "eq(dis(_,_),uni(_),0)."),
   ("\\<open>\\<And>p q p'. Uni p' = Dis p q \\<equiv> False\\<close>",
    "eq(uni(_),dis(_,_),0)."),
   ("\\<open>\\<And>p q p'. Dis p q = Exi p' \\<equiv> False\\<close>",
    "eq(dis(_,_),exi(_),0)."),
   ("\\<open>\\<And>p q p'. Exi p' = Dis p q \\<equiv> False\\<close>",
    "eq(exi(_),dis(_,_),0)."),
   ("\\<open>\\<And>p p'. Uni p = Exi p' \\<equiv> False\\<close>",
    "eq(uni(_),exi(_),0)."),
   ("\\<open>\\<And>p p'. Exi p' = Uni p \\<equiv> False\\<close>",
    "eq(exi(_),uni(_),0)."),
   ("\\<open>\\<And>b i v b' i' v'. Pre b i v = Pre b' i' v' \\<equiv> b = b' \\<and> i = i' \\<and> v = v'\\<close>",
    "eq(pre(B,I,V),pre(B_,I_,V_),Y) :- eq(B,B_,X0), eq(I,I_,X1), conj(X0,X1,X2), eq(V,V_,X3), conj(X2,X3,Y)."),
   ("\\<open>\\<And>p q p' q'. Con p q = Con p' q' \\<equiv> p = p' \\<and> q = q'\\<close>",
    "eq(con(P,Q),con(P_,Q_),Y) :- eq(P,P_,X0), eq(Q,Q_,X1), conj(X0,X1,Y)."),
   ("\\<open>\\<And>p q p' q'. Dis p q = Dis p' q' \\<equiv> p = p' \\<and> q = q'\\<close>",
    "eq(dis(P,Q),dis(P_,Q_),Y) :- eq(P,P_,X0), eq(Q,Q_,X1), conj(X0,X1,Y)."),
   ("\\<open>\\<And>p p'. Uni p = Uni p' \\<equiv> p = p'\\<close>",
    "eq(uni(P),uni(P_),Y) :- eq(P,P_,Y)."),
   ("\\<open>\\<And>p p'. Exi p = Exi p' \\<equiv> p = p'\\<close>",
    "eq(exi(P),exi(P_),Y) :- eq(P,P_,Y)."),
   ("\\<open>\\<And>x. add x 0 \\<equiv> x\\<close>",
    "add(X,0,X)."),
   ("\\<open>\\<And>x n. add x (Suc n) \\<equiv> Suc (add x n)\\<close>",
    "add(X,suc(N),suc(X0)) :- add(X,N,X0)."),
   ("\\<open>\\<And>x. sub x 0 \\<equiv> x\\<close>",
    "sub(X,0,X)."),
   ("\\<open>\\<And>x n. sub x (Suc n) \\<equiv> dec (sub x n)\\<close>",
    "sub(X,suc(N),Y) :- sub(X,N,X0), dec(X0,Y)."),
   ("\\<open>dec 0 \\<equiv> 0\\<close>",
    "dec(0,0)."),
   ("\\<open>\\<And>n. dec (Suc n) \\<equiv> n\\<close>",
    "dec(suc(N),N)."),
   ("\\<open>\\<And>l. [] @ l \\<equiv> l\\<close>",
    "append([],L,L)."),
   ("\\<open>\\<And>h t l. (h # t) @ l \\<equiv> h # t @ l\\<close>",
    "append([H|T],L,[H|X0]) :- append(T,L,X0)."),
   ("\\<open>\\<And>x y. (if True then x else y) \\<equiv> x\\<close>",
    "ifelse(1,X,_,X)."),
   ("\\<open>\\<And>x y. (if False then x else y) \\<equiv> y\\<close>",
    "ifelse(0,_,Y,Y)."),
   ("\\<open>\\<not> True \\<equiv> False\\<close>",
    "not(1,0)."),
   ("\\<open>\\<not> False \\<equiv> True\\<close>",
    "not(0,1)."),
   ("\\<open>\\<And>x y. fst (x,y) \\<equiv> x\\<close>",
    "fst((X,_),X)."),
   ("\\<open>\\<And>x y. snd (x,y) \\<equiv> y\\<close>",
    "snd((_,Y),Y)."),
   ("\\<open>0 = 0 \\<equiv> True\\<close>",
    "eq(0,0,1)."),
   ("\\<open>[] = [] \\<equiv> True\\<close>",
    "eq([],[],1)."),
   ("\\<open>True = True \\<equiv> True\\<close>",
    "eq(1,1,1)."),
   ("\\<open>False = False \\<equiv> True\\<close>",
    "eq(0,0,1)."),
   ("\\<open>\\<And>b. True \\<and> b \\<equiv> b\\<close>",
    "conj(1,B,B)."),
   ("\\<open>\\<And>b. False \\<and> b \\<equiv> False\\<close>",
    "conj(0,_,0)."),
   ("\\<open>\\<And>n. 0 = Suc n \\<equiv> False\\<close>",
    "eq(0,suc(_),0)."),
   ("\\<open>\\<And>n. Suc n = 0 \\<equiv> False\\<close>",
    "eq(suc(_),0,0)."),
   ("\\<open>\\<And>h t. [] = h # t \\<equiv> False\\<close>",
    "eq([],[_|_],0)."),
   ("\\<open>\\<And>h t. h # t = [] \\<equiv> False\\<close>",
    "eq([_|_],[],0)."),
   ("\\<open>True = False \\<equiv> False\\<close>",
    "eq(1,0,0)."),
   ("\\<open>False = True \\<equiv> False\\<close>",
    "eq(0,1,0)."),
   ("\\<open>\\<And>x y x' y'. (x,y) = (x',y') \\<equiv> x = x' \\<and> y = y'\\<close>",
    "eq((X,Y),(X_,Y_),Y) :- eq(X,X_,X0), eq(Y,Y_,X1), conj(X0,X1,Y)."),
   ("\\<open>\\<And>n n'. Suc n = Suc n' \\<equiv> n = n'\\<close>",
    "eq(suc(N),suc(N_),Y) :- eq(N,N_,Y)."),
   ("\\<open>\\<And>h t h' t'. h # t = h' # t' \\<equiv> h = h' \\<and> t = t'\\<close>",
    "eq([H|T],[H_|T_],Y) :- eq(H,H_,X0), eq(T,T_,X1), conj(X0,X1,Y).")]
