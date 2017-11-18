\<open>\<And>p. check p \<equiv> prover [[(0,p)]]\<close>
\<open>\<And>h t. prover (h # t) \<equiv> prover (solves (h # t))\<close>
\<open>prover [] \<equiv> True\<close>
\<open>solves [] \<equiv> []\<close>
\<open>\<And>h t. solves (h # t) \<equiv> solve h @ solves t\<close>
\<open>solve [] \<equiv> [[]]\<close>
\<open>\<And>h t. solve (h # t) \<equiv> track t (fst h) (snd h)\<close>
\<open>\<And>s n b i v. track s n (Pre b i v) \<equiv> stop [s @ [(0,Pre b i v)]] (Pre (\<not> b) i v) (base s)\<close>
\<open>\<And>s n p q. track s n (Con p q) \<equiv> [s @ [(0,p)],s @ [(0,q)]]\<close>
\<open>\<And>s n p q. track s n (Dis p q) \<equiv> [s @ [(0,p),(0,q)]]\<close>
\<open>\<And>s n p. track s n (Uni p) \<equiv> [s @ [(0,subst 0 (fresh (frees (Uni p # base s))) p)]]\<close>
\<open>\<And>s n p. track s n (Exi p) \<equiv> [s @ [(0,subst 0 n p),(Suc n,Exi p)]]\<close>
\<open>\<And>c p. stop c p [] \<equiv> c\<close>
\<open>\<And>c p h t. stop c p (h # t) \<equiv> (if p = h then [] else stop c p t)\<close>
\<open>base [] \<equiv> []\<close>
\<open>\<And>h t. base (h # t) \<equiv> snd h # base t\<close>
\<open>\<And>x s b i v. subst x s (Pre b i v) \<equiv> Pre b i (mend x s v)\<close>
\<open>\<And>x s p q. subst x s (Con p q) \<equiv> Con (subst x s p) (subst x s q)\<close>
\<open>\<And>x s p q. subst x s (Dis p q) \<equiv> Dis (subst x s p) (subst x s q)\<close>
\<open>\<And>x s p. subst x s (Uni p) \<equiv> Uni (subst (Suc x) (Suc s) p)\<close>
\<open>\<And>x s p. subst x s (Exi p) \<equiv> Exi (subst (Suc x) (Suc s) p)\<close>
\<open>\<And>x s. mend x s [] \<equiv> []\<close>
\<open>\<And>x s h t. mend x s (h # t) \<equiv> more x s h (sub h x) # mend x s t\<close>
\<open>\<And>x s h. more x s h 0 \<equiv> over s h (sub x h)\<close>
\<open>\<And>x s h n. more x s h (Suc n) \<equiv> dec h\<close>
\<open>\<And>s h. over s h 0 \<equiv> s\<close>
\<open>\<And>s h n. over s h (Suc n) \<equiv> h\<close>
\<open>fresh [] \<equiv> 0\<close>
\<open>\<And>h t. fresh (h # t) \<equiv> Suc (add (sub (dec (fresh t)) h) h)\<close>
\<open>frees [] \<equiv> []\<close>
\<open>\<And>h t. frees (h # t) \<equiv> free h @ frees t\<close>
\<open>\<And>b i v. free (Pre b i v) \<equiv> v\<close>
\<open>\<And>p q. free (Con p q) \<equiv> free p @ free q\<close>
\<open>\<And>p q. free (Dis p q) \<equiv> free p @ free q\<close>
\<open>\<And>p. free (Uni p) \<equiv> dump (free p)\<close>
\<open>\<And>p. free (Exi p) \<equiv> dump (free p)\<close>
\<open>dump [] \<equiv> []\<close>
\<open>\<And>h t. dump (h # t) \<equiv> dash (dump t) h\<close>
\<open>\<And>l. dash l 0 \<equiv> l\<close>
\<open>\<And>l n. dash l (Suc n) \<equiv> n # l\<close>

\<open>\<And>b i v p q. Pre b i v = Con p q \<equiv> False\<close>
\<open>\<And>b i v p q. Con p q = Pre b i v \<equiv> False\<close>
\<open>\<And>b i v p q. Pre b i v = Dis p q \<equiv> False\<close>
\<open>\<And>b i v p q. Dis p q = Pre b i v \<equiv> False\<close>
\<open>\<And>b i v p. Pre b i v = Uni p \<equiv> False\<close>
\<open>\<And>b i v p. Uni p = Pre b i v \<equiv> False\<close>
\<open>\<And>b i v p. Pre b i v = Exi p \<equiv> False\<close>
\<open>\<And>b i v p. Exi p = Pre b i v \<equiv> False\<close>
\<open>\<And>p q p' q'. Con p q = Dis p' q' \<equiv> False\<close>
\<open>\<And>p q p' q'. Dis p' q' = Con p q \<equiv> False\<close>
\<open>\<And>p q p'. Con p q = Uni p' \<equiv> False\<close>
\<open>\<And>p q p'. Uni p' = Con p q \<equiv> False\<close>
\<open>\<And>p q p'. Con p q = Exi p' \<equiv> False\<close>
\<open>\<And>p q p'. Exi p' = Con p q \<equiv> False\<close>
\<open>\<And>p q p'. Dis p q = Uni p' \<equiv> False\<close>
\<open>\<And>p q p'. Uni p' = Dis p q \<equiv> False\<close>
\<open>\<And>p q p'. Dis p q = Exi p' \<equiv> False\<close>
\<open>\<And>p q p'. Exi p' = Dis p q \<equiv> False\<close>
\<open>\<And>p p'. Uni p = Exi p' \<equiv> False\<close>
\<open>\<And>p p'. Exi p' = Uni p \<equiv> False\<close>
\<open>\<And>b i v b' i' v'. Pre b i v = Pre b' i' v' \<equiv> b = b' \<and> i = i' \<and> v = v'\<close>
\<open>\<And>p q p' q'. Con p q = Con p' q' \<equiv> p = p' \<and> q = q'\<close>
\<open>\<And>p q p' q'. Dis p q = Dis p' q' \<equiv> p = p' \<and> q = q'\<close>
\<open>\<And>p p'. Uni p = Uni p' \<equiv> p = p'\<close>
\<open>\<And>p p'. Exi p = Exi p' \<equiv> p = p'\<close>

\<open>\<And>x. add x 0 \<equiv> x\<close>
\<open>\<And>x n. add x (Suc n) \<equiv> Suc (add x n)\<close>
\<open>\<And>x. sub x 0 \<equiv> x\<close>
\<open>\<And>x n. sub x (Suc n) \<equiv> dec (sub x n)\<close>
\<open>dec 0 \<equiv> 0\<close>
\<open>\<And>n. dec (Suc n) \<equiv> n\<close>
\<open>\<And>l. [] @ l \<equiv> l\<close>
\<open>\<And>h t l. (h # t) @ l \<equiv> h # t @ l\<close>
\<open>\<And>x y. if True then x else y \<equiv> x\<close>
\<open>\<And>x y. if False then x else y \<equiv> y\<close>
\<open>\<not> True \<equiv> False\<close>
\<open>\<not> False \<equiv> True\<close>
\<open>\<And>x y. fst (x,y) \<equiv> x\<close>
\<open>\<And>x y. snd (x,y) \<equiv> y\<close>
\<open>0 = 0 \<equiv> True\<close>
\<open>[] = [] \<equiv> True\<close>
\<open>True = True \<equiv> True\<close>
\<open>False = False \<equiv> True\<close>
\<open>\<And>b. True \<and> b \<equiv> b\<close>
\<open>\<And>b. False \<and> b \<equiv> False\<close>
\<open>\<And>n. 0 = Suc n \<equiv> False\<close>
\<open>\<And>n. Suc n = 0 \<equiv> False\<close>
\<open>\<And>h t. [] = h # t \<equiv> False\<close>
\<open>\<And>h t. h # t = [] \<equiv> False\<close>
\<open>True = False \<equiv> False\<close>
\<open>False = True \<equiv> False\<close>
\<open>\<And>x y x' y'. (x,y) = (x',y') \<equiv> x = x' \<and> y = y'\<close>
\<open>\<And>n n'. Suc n = Suc n' \<equiv> n = n'\<close>
\<open>\<And>h t h' t'. h # t = h' # t' \<equiv> h = h' \<and> t = t'\<close>
