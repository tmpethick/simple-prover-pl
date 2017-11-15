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
