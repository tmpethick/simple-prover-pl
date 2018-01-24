{-# LANGUAGE RankNTypes #-}
-- Export from Isabelle with extended Nnf to allow for more than NNF
-- with supplimentary function `nnf` which converts structure to NNF.
-- This is used to run test cases not in NNF to debug the converter.
module Prover(Nat, Nnf, test, testCases, check) where {

import Prelude ((==), (/=), (<), (<=), (>=), (>), (+), (-), (*), (/), (**),
  (>>=), (>>), (=<<), (&&), (||), (^), (^^), (.), ($), ($!), (++), (!!), Eq,
  error, id, return, not, fst, snd, map, filter, concat, concatMap, reverse,
  zip, null, takeWhile, dropWhile, all, any, Integer, negate, abs, divMod,
  String, Bool(True, False), Maybe(Nothing, Just), Show);
import qualified Prelude;

data Nat = Zero_nat | Suc Nat deriving Show;

equal_nat :: Nat -> Nat -> Bool;
equal_nat Zero_nat (Suc x2) = False;
equal_nat (Suc x2) Zero_nat = False;
equal_nat (Suc x2) (Suc y2) = equal_nat x2 y2;
equal_nat Zero_nat Zero_nat = True;

instance Eq Nat where {
  a == b = equal_nat a b;
};

data Nnf = 
  Pre Bool Nat [Nat] | 
  Con Nnf Nnf | 
  Dis Nnf Nnf | 
  Uni Nnf | 
  Exi Nnf |
  Neg Nnf |
  Iff Nnf Nnf |
  If Nnf Nnf |
  Imp Nnf Nnf deriving Show;

flip False = True;
flip True = False;

nnfConn (Neg p)   = Neg (nnfConn p);
nnfConn (Dis p q) = Dis (nnfConn p) (nnfConn q);
nnfConn (Con p q) = Con (nnfConn p) (nnfConn q);
nnfConn (Iff p q) = nnfConn (Con (Imp p q) (Imp p q));
nnfConn (Imp p q) = Dis (Neg (nnfConn p)) (nnfConn q);
nnfConn (If p q)  = Dis (nnfConn p) (Neg (nnfConn q));
nnfConn (Uni p)   = Uni (nnfConn p);
nnfConn (Exi p)   = Exi (nnfConn p);
nnfConn p         = p;

nnfPushNeg (Neg (Pre b t v)) = Pre (flip b) t v;
nnfPushNeg (Neg (Neg p))     = nnfPushNeg p;
nnfPushNeg (Neg (Dis p q))   = Con (nnfPushNeg (Neg p)) (nnfPushNeg (Neg q));
nnfPushNeg (Neg (Con p q))   = Dis (nnfPushNeg (Neg p)) (nnfPushNeg (Neg q));
nnfPushNeg (Neg (Uni p))     = Exi (nnfPushNeg (Neg p));
nnfPushNeg (Neg (Exi p))     = Uni (nnfPushNeg (Neg p));
nnfPushNeg (Dis p q)         = Dis (nnfPushNeg p) (nnfPushNeg q);
nnfPushNeg (Con p q)         = Con (nnfPushNeg p) (nnfPushNeg q);
nnfPushNeg (Uni p)           = Uni (nnfPushNeg p);  
nnfPushNeg (Exi p)           = Exi (nnfPushNeg p);
nnfPushNeg p                 = p;

nnf = nnfPushNeg . nnfConn;

add :: Nat -> Nat -> Nat;
add x Zero_nat = x;
add x (Suc n) = Suc (add x n);

dec :: Nat -> Nat;
dec Zero_nat = Zero_nat;
dec (Suc n) = n;

sub :: Nat -> Nat -> Nat;
sub x Zero_nat = x;
sub x (Suc n) = dec (sub x n);

base :: [(Nat, Nnf)] -> [Nnf];
base [] = [];
base (h : t) = snd h : base t;

dash :: [Nat] -> Nat -> [Nat];
dash l Zero_nat = l;
dash l (Suc n) = n : l;

dump :: [Nat] -> [Nat];
dump [] = [];
dump (h : t) = dash (dump t) h;

free :: Nnf -> [Nat];
free (Pre uu uv v) = v;
free (Con p q) = free p ++ free q;
free (Dis p q) = free p ++ free q;
free (Uni p) = dump (free p);
free (Exi p) = dump (free p);

over :: Nat -> Nat -> Nat -> Nat;
over s uu Zero_nat = s;
over uv h (Suc uw) = h;

more :: Nat -> Nat -> Nat -> Nat -> Nat;
more x s h Zero_nat = over s h (sub x h);
more uu uv h (Suc uw) = dec h;

mend :: Nat -> Nat -> [Nat] -> [Nat];
mend uu uv [] = [];
mend x s (h : t) = more x s h (sub h x) : mend x s t;

subst :: Nat -> Nat -> Nnf -> Nnf;
subst x s (Pre b i v) = Pre b i (mend x s v);
subst x s (Con p q) = Con (subst x s p) (subst x s q);
subst x s (Dis p q) = Dis (subst x s p) (subst x s q);
subst x s (Uni p) = Uni (subst (Suc x) (Suc s) p);
subst x s (Exi p) = Exi (subst (Suc x) (Suc s) p);

fresh :: [Nat] -> Nat;
fresh [] = Zero_nat;
fresh (h : t) = Suc (add (sub (dec (fresh t)) h) h);

frees :: [Nnf] -> [Nat];
frees [] = [];
frees (h : t) = free h ++ frees t;

equal_nnf :: Nnf -> Nnf -> Bool;
equal_nnf (Uni x4) (Exi x5) = False;
equal_nnf (Exi x5) (Uni x4) = False;
equal_nnf (Dis x31 x32) (Exi x5) = False;
equal_nnf (Exi x5) (Dis x31 x32) = False;
equal_nnf (Dis x31 x32) (Uni x4) = False;
equal_nnf (Uni x4) (Dis x31 x32) = False;
equal_nnf (Con x21 x22) (Exi x5) = False;
equal_nnf (Exi x5) (Con x21 x22) = False;
equal_nnf (Con x21 x22) (Uni x4) = False;
equal_nnf (Uni x4) (Con x21 x22) = False;
equal_nnf (Con x21 x22) (Dis x31 x32) = False;
equal_nnf (Dis x31 x32) (Con x21 x22) = False;
equal_nnf (Pre x11 x12 x13) (Exi x5) = False;
equal_nnf (Exi x5) (Pre x11 x12 x13) = False;
equal_nnf (Pre x11 x12 x13) (Uni x4) = False;
equal_nnf (Uni x4) (Pre x11 x12 x13) = False;
equal_nnf (Pre x11 x12 x13) (Dis x31 x32) = False;
equal_nnf (Dis x31 x32) (Pre x11 x12 x13) = False;
equal_nnf (Pre x11 x12 x13) (Con x21 x22) = False;
equal_nnf (Con x21 x22) (Pre x11 x12 x13) = False;
equal_nnf (Exi x5) (Exi y5) = equal_nnf x5 y5;
equal_nnf (Uni x4) (Uni y4) = equal_nnf x4 y4;
equal_nnf (Dis x31 x32) (Dis y31 y32) = equal_nnf x31 y31 && equal_nnf x32 y32;
equal_nnf (Con x21 x22) (Con y21 y22) = equal_nnf x21 y21 && equal_nnf x22 y22;
equal_nnf (Pre x11 x12 x13) (Pre y11 y12 y13) =
  x11 == y11 && equal_nat x12 y12 && x13 == y13;

stop :: [[(Nat, Nnf)]] -> Nnf -> [Nnf] -> [[(Nat, Nnf)]];
stop c uu [] = c;
stop c p (h : t) = (if equal_nnf p h then [] else stop c p t);

track :: [(Nat, Nnf)] -> Nat -> Nnf -> [[(Nat, Nnf)]];
track s uu (Pre b i v) =
  stop [s ++ [(Zero_nat, Pre b i v)]] (Pre (not b) i v) (base s);
track s uv (Con p q) = [s ++ [(Zero_nat, p)], s ++ [(Zero_nat, q)]];
track s uw (Dis p q) = [s ++ [(Zero_nat, p), (Zero_nat, q)]];
track s ux (Uni p) =
  [s ++ [(Zero_nat, subst Zero_nat (fresh (frees (Uni p : base s))) p)]];
track s n (Exi p) = [s ++ [(Zero_nat, subst Zero_nat n p), (Suc n, Exi p)]];

solve :: [(Nat, Nnf)] -> [[(Nat, Nnf)]];
solve [] = [[]];
solve (h : t) = track t (fst h) (snd h);

solves :: [[(Nat, Nnf)]] -> [[(Nat, Nnf)]];
solves [] = [];
solves (h : t) = solve h ++ solves t;

nulla :: forall a. [a] -> Bool;
nulla [] = True;
nulla (uu : uv) = False;

main ::
  (([[(Nat, Nnf)]] -> Bool) ->
    ([[(Nat, Nnf)]] -> [[(Nat, Nnf)]]) -> [[(Nat, Nnf)]] -> Bool) ->
    Nnf -> Bool;
main a p = a nulla solves [[(Zero_nat, p)]];

test :: Nnf;
test =
  Dis (Uni (Con (Pre False Zero_nat [Zero_nat])
             (Pre False (Suc Zero_nat) [Zero_nat])))
    (Dis (Exi (Pre True (Suc Zero_nat) [Zero_nat]))
      (Exi (Pre True Zero_nat [Zero_nat])));

iterator :: forall a. (a -> Bool) -> (a -> a) -> a -> Bool;
iterator g f c = (if g c then True else iterator g f (f c));

check :: Nnf -> Bool;
check = main iterator;

testCases = [
  Imp (Pre False Zero_nat []) (Pre False Zero_nat []),
  Dis (Iff (Pre False Zero_nat []) (Pre False Zero_nat [])) (Pre False (Suc Zero_nat) []),
  Imp (Uni(Pre False Zero_nat [Zero_nat, Zero_nat])) (Uni(Exi(Pre False Zero_nat [Zero_nat, Suc Zero_nat])))
  If (Uni(Exi(Pre False Zero_nat [Zero_nat, Suc Zero_nat]))) (Uni(Pre False Zero_nat [Zero_nat, Zero_nat])),
  Imp (Imp (Pre False (Suc Zero_nat) []) (Pre False (Suc (Suc Zero_nat)) [])) (Imp (Imp (Pre True (Suc Zero_nat) []) (Pre True Zero_nat [])) (Imp (Pre False Zero_nat []) (Pre False (Suc (Suc Zero_nat)) [])))]
  
  -- (((Pre False False []) Con (Pre False Suc(False) [])) Imp (Pre False Suc(Suc(False)) [])) Imp ((Pre False False []) Imp ((Pre False Suc(False) []) Imp (Pre False Suc(Suc(False)) []))),
  -- (Pre True False []) Imp (Pre True False []),
  -- ((Pre False False []) Imp ((Pre False Suc(False) []) Imp (Pre False Suc(Suc(False)) []))) Imp (((Pre False False []) Con (Pre False Suc(False) [])) Imp (Pre False Suc(Suc(False)) [])),
  -- ((Pre False False []) Imp (Pre False Suc(False) [])) Imp (((Pre False False []) Con (Pre False Suc(Suc(False)) [])) Imp ((Pre False Suc(False) []) Con (Pre False Suc(Suc(False)) []))),
  -- ((Pre False False []) Dis (Pre False Suc(False) [])) Imp ((Pre False Suc(False) []) Dis (Pre False False [])),
  -- ((Pre False Suc(False) []) Imp (Pre False Suc(Suc(False)) [])) Imp (((Pre False False []) Dis (Pre False Suc(False) [])) Imp ((Pre False False []) Dis (Pre False Suc(Suc(False)) []))),
  -- (((Pre False False []) Dis (Pre False Suc(False) [])) Dis (Pre False Suc(Suc(False)) [])) Imp ((Pre False False []) Dis ((Pre False Suc(False) []) Dis (Pre False Suc(Suc(False)) []))),
  -- (Pre False False []) Imp ((Pre False Suc(False) []) Imp (Pre False False [])),
  -- ((Pre True False []) Dis (Pre False Suc(False) [])) Imp ((Pre False False []) Imp (Pre False Suc(False) [])),
  -- (((Pre False False []) Imp (Pre False Suc(False) [])) Con ((Pre False False []) Imp (Pre True Suc(False) []))) Imp ((Pre True False [])),
  -- ((Pre False False []) Imp (Pre True False [])) Imp (Pre True False []),
  -- (((Pre False False []) Imp ((Pre False Suc(False) []) Imp (Pre False Suc(Suc(False)) []))) Con ((Pre False False []) Con (Pre True Suc(Suc(False)) []))) Imp ((Pre True Suc(False) [])),
  -- ((((Pre False False []) Con (Pre True Suc(False) [])) Imp (Pre False Suc(Suc(False)) [])) Con (((Pre True Suc(Suc(False)) [])) Con (Pre False False []))) Imp (Pre False Suc(False) []),
  -- (((Pre False False []) Imp (Pre False Suc(False) [])) Con ((Pre True Suc(False) []))) Imp (Pre True False []),
  -- (Pre False False []) Imp ((Pre False False [])),
  -- (Pre False False []) Dis (Pre True False []),
  -- ((Pre False False []) Imp (Pre False Suc(False) [])) Imp ((Pre True False []) Dis (Pre False Suc(False) [])),
  -- ((((Pre False False []) Con (Pre False Suc(False) [])) Imp (Pre False Suc(Suc(False)) [])) Con (Pre False False [])) Imp ((Pre False Suc(False) []) Imp (Pre False Suc(Suc(False)) [])),
  -- ((Uni((Pre False False [Zero_nat]) Imp (Pre False Suc(False) [Zero_nat]))) Con (Uni((Pre False False [Zero_nat])))) Imp (Uni((Pre False Suc(False) [Zero_nat]))),
  -- ((Pre False False [Zero_nat]) Con (Uni((Pre False False [Zero_nat]) Imp (Pre True Suc(False) [False))))) Imp ((Pre True Suc(False) [False))),
  -- Uni((Pre False False [Zero_nat])) Imp Exi((Pre False False [Zero_nat])),
  -- ((Uni((Pre False False [Zero_nat]) Imp (Pre False Suc(False) [Zero_nat]))) Con (Exi((Pre False False [Zero_nat])))) Imp (Exi((Pre False Suc(False) [Zero_nat]))),
  -- ((Uni((Pre False Suc(False) [Zero_nat]) Imp (Pre False Suc(Suc(False)) [Zero_nat]))) Con (Exi((Pre False False [Zero_nat]) Con (Pre False Suc(False) [Zero_nat])))) Imp (Exi((Pre False False [Zero_nat]) Con (Pre False Suc(Suc(False)) [Zero_nat]))),
  -- (Exi((Pre False False [Zero_nat])) Con Uni(Uni((Pre False False [Zero_nat]) Imp (Pre False Suc(False) [True])))) Imp (Uni((Pre False Suc(False) [Zero_nat]))),
  -- ((Pre True False [] Con (Pre False Suc(False) []))) Imp ((Pre True False []) Dis (Pre True Suc(False) []))]

-- check . nnf $ testCases !! 2

}
