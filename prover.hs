
import Prelude ((==), (/=), (<), (<=), (>=), (>), (+), (-), (*), (/), (**),
  (>>=), (>>), (=<<), (&&), (||), (^), (^^), (.), ($), ($!), (++), (!!), Eq, Show,
  error, id, return, not, fst, snd, map, filter, concat, concatMap, reverse,
  zip, null, takeWhile, dropWhile, all, any, Integer, negate, abs, divMod,
  String, Bool(True, False), Maybe(Nothing, Just));
import qualified Prelude;

data Nat = ZeroNat | Suc Nat deriving Prelude.Show;

equalNat :: Nat -> Nat -> Bool;
equalNat ZeroNat (Suc x2) = False;
equalNat (Suc x2) ZeroNat = False;
equalNat (Suc x2) (Suc y2) = equalNat x2 y2;
equalNat ZeroNat ZeroNat = True;

instance Eq Nat where {
  a == b = equalNat a b;
};

data Nnf = Pre Bool Nat [Nat] | Con Nnf Nnf | Dis Nnf Nnf | Uni Nnf | Exi Nnf deriving Prelude.Show;


add :: Nat -> Nat -> Nat;
add x ZeroNat = x;
add x (Suc n) = Suc (add x n);

dec :: Nat -> Nat;
dec ZeroNat = ZeroNat;
dec (Suc n) = n;

sub :: Nat -> Nat -> Nat;
sub x ZeroNat = x;
sub x (Suc n) = dec (sub x n);

base :: [(Nat, Nnf)] -> [Nnf];
base [] = [];
base (h : t) = snd h : base t;

dash :: [Nat] -> Nat -> [Nat];
dash l ZeroNat = l;
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
over s uu ZeroNat = s;
over uv h (Suc uw) = h;

more :: Nat -> Nat -> Nat -> Nat -> Nat;
more x s h ZeroNat = over s h (sub x h);
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
fresh [] = ZeroNat;
fresh (h : t) = Suc (add (sub (dec (fresh t)) h) h);

frees :: [Nnf] -> [Nat];
frees [] = [];
frees (h : t) = free h ++ frees t;

equalNnf :: Nnf -> Nnf -> Bool;
equalNnf (Uni x4) (Exi x5) = False;
equalNnf (Exi x5) (Uni x4) = False;
equalNnf (Dis x31 x32) (Exi x5) = False;
equalNnf (Exi x5) (Dis x31 x32) = False;
equalNnf (Dis x31 x32) (Uni x4) = False;
equalNnf (Uni x4) (Dis x31 x32) = False;
equalNnf (Con x21 x22) (Exi x5) = False;
equalNnf (Exi x5) (Con x21 x22) = False;
equalNnf (Con x21 x22) (Uni x4) = False;
equalNnf (Uni x4) (Con x21 x22) = False;
equalNnf (Con x21 x22) (Dis x31 x32) = False;
equalNnf (Dis x31 x32) (Con x21 x22) = False;
equalNnf (Pre x11 x12 x13) (Exi x5) = False;
equalNnf (Exi x5) (Pre x11 x12 x13) = False;
equalNnf (Pre x11 x12 x13) (Uni x4) = False;
equalNnf (Uni x4) (Pre x11 x12 x13) = False;
equalNnf (Pre x11 x12 x13) (Dis x31 x32) = False;
equalNnf (Dis x31 x32) (Pre x11 x12 x13) = False;
equalNnf (Pre x11 x12 x13) (Con x21 x22) = False;
equalNnf (Con x21 x22) (Pre x11 x12 x13) = False;
equalNnf (Exi x5) (Exi y5) = equalNnf x5 y5;
equalNnf (Uni x4) (Uni y4) = equalNnf x4 y4;
equalNnf (Dis x31 x32) (Dis y31 y32) = equalNnf x31 y31 && equalNnf x32 y32;
equalNnf (Con x21 x22) (Con y21 y22) = equalNnf x21 y21 && equalNnf x22 y22;
equalNnf (Pre x11 x12 x13) (Pre y11 y12 y13) =
  x11 == y11 && equalNat x12 y12 && x13 == y13;

stop :: [[(Nat, Nnf)]] -> Nnf -> [Nnf] -> [[(Nat, Nnf)]];
stop c uu [] = c;
stop c p (h : t) = if equalNnf p h then [] else stop c p t;

track :: [(Nat, Nnf)] -> Nat -> Nnf -> [[(Nat, Nnf)]];
track s uu (Pre b i v) =
  stop [s ++ [(ZeroNat, Pre b i v)]] (Pre (not b) i v) (base s);
track s uv (Con p q) = [s ++ [(ZeroNat, p)], s ++ [(ZeroNat, q)]];
track s uw (Dis p q) = [s ++ [(ZeroNat, p), (ZeroNat, q)]];
track s ux (Uni p) =
  [s ++ [(ZeroNat, subst ZeroNat (fresh (frees (Uni p : base s))) p)]];
track s n (Exi p) = [s ++ [(ZeroNat, subst ZeroNat n p), (Suc n, Exi p)]];

solve :: [(Nat, Nnf)] -> [[(Nat, Nnf)]];
solve [] = [[]];
solve (h : t) = track t (fst h) (snd h);

solves :: [[(Nat, Nnf)]] -> [[(Nat, Nnf)]];
solves [] = [];
solves (h : t) = solve h ++ solves t;

nulla :: [a] -> Bool;
nulla [] = True;
nulla (uu : uv) = False;

main' ::
  (([[(Nat, Nnf)]] -> Bool) ->
    ([[(Nat, Nnf)]] -> [[(Nat, Nnf)]]) -> [[(Nat, Nnf)]] -> Bool) ->
    Nnf -> Bool;
main' a p = a nulla solves [[(ZeroNat, p)]];

test :: Nnf;
test =
  Dis (Uni (Con (Pre False ZeroNat [ZeroNat])
              (Pre False (Suc ZeroNat) [ZeroNat])))
    (Dis (Exi (Pre True (Suc ZeroNat) [ZeroNat]))
      (Exi (Pre True ZeroNat [ZeroNat])));

iterator :: (a -> Bool) -> (a -> a) -> a -> Bool;
iterator g f c = if g c then True else iterator g f (f c);

check :: Nnf -> Bool;
check = main' iterator;

-- let p = Dis (Pre True ZeroNat [ZeroNat]) (Pre False ZeroNat [ZeroNat])
-- track ([(ZeroNat, p)]) ZeroNat p


-- let pp = [(ZeroNat,Pre False ZeroNat [ZeroNat])], Pre True ZeroNat [ZeroNat] => []
