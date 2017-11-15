{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}

module PrettyProlog where

import Data.Eq.Deriving (deriveEq1)
import Text.Show.Deriving (deriveShow1)
import Data.Functor.Identity (Identity)  
import Data.Char
import Control.Arrow ((>>>))
import Text.PrettyPrint.Leijen
import qualified Control.Comonad.Trans.Cofree as C
import Control.Comonad.Cofree (Cofree((:<)))
import Control.Monad.Free
import Data.Functor.Foldable
import Control.Monad.Supply (Supply, supply, evalSupply)

import Foldable (cataM)
import Parse (
  Term,
  TermF(TermBinOp, TermTerOp, TupleTerm, ConstTerm, ListTerm, VarTerm),
  TVar(TId, Wildcard),
  TConst(TTrue, TFalse, TString, TInteger),
  TBinOp(TFunc, TEquiv, TAddHead, TConcat, TConj),
  TTerOp(TIf))

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

data PVar = PId String | PWildcard deriving (Eq, Show)

data PConst = PTrue | PFalse | PString String | PInteger Integer deriving (Eq, Show)

data PBinOp = PAddHead | PImpl deriving (Eq, Show)

data PTerOp = PIf deriving (Eq, Show)

data PTermF a = 
    ConstPTerm PConst
  | VarPTerm PVar
  | TuplePTerm [a]
  | ListPTerm [a]
  | PTermBinOp PBinOp a a
  | PTermTerOp PTerOp a a a
  | PAnd [a]
  | PFuncApp PVar [a]
  | PPredicate PVar [a]
  deriving (Show, Eq, Functor, Foldable, Traversable)

deriveShow1 ''PTermF
deriveEq1 ''PTermF

type PTerm = Fix PTermF

constPTerm :: PConst -> PTerm
constPTerm = Fix . ConstPTerm

varPTerm :: PVar -> PTerm
varPTerm = Fix . VarPTerm

tuplePTerm :: [PTerm] -> PTerm
tuplePTerm = Fix . TuplePTerm

listPTerm :: [PTerm] -> PTerm
listPTerm = Fix . ListPTerm

pTermBinOp :: PBinOp -> PTerm -> PTerm -> PTerm
pTermBinOp o a b = Fix $ PTermBinOp o a b

pTermTerOp :: PTerOp -> PTerm -> PTerm -> PTerm -> PTerm
pTermTerOp o a b c = Fix $ PTermTerOp o a b c

pAnd :: [PTerm] -> PTerm
pAnd = Fix . PAnd

pFuncApp :: PVar -> [PTerm] -> PTerm
pFuncApp v a = Fix $ PFuncApp v a

pPredicate :: PVar -> [PTerm] -> PTerm
pPredicate v a = Fix $ PPredicate v a

-------------------------------------------------------------------------------
-- To Prolog AST
-------------------------------------------------------------------------------

-------- AST convertion ----------

toPrologAST :: Term -> PTerm
toPrologAST = cata (Fix . alg) where
  -- Convert nested one arg function applications
  alg (TermBinOp TFunc a b) = case unfix a of 
    (VarPTerm f) -> PFuncApp f [b]
    (PFuncApp f b') -> PFuncApp f (b' ++ [b])
  
  -- Concat is just a function call
  alg (TermBinOp TConcat a b) = PFuncApp (PId "append") [a, b]

  -- 1-to-1 translations
  alg (ConstTerm c) = ConstPTerm (convertConst c)
  alg (VarTerm v) = VarPTerm (convertVar v)
  alg (TupleTerm as) = TuplePTerm as
  alg (ListTerm as) = ListPTerm as
  alg (TermBinOp o a b) = PTermBinOp (convertBinOp o) a b
  alg (TermTerOp o a b c) = PTermTerOp (convertTerOp o) a b c

convertBinOp TAddHead = PAddHead
-- TODO: convertBinOp TConj = 
convertBinOp TEquiv = PImpl

convertTerOp TIf = PIf

convertVar (TId id) = PId id
convertVar Wildcard = PWildcard
  
convertConst TTrue = PTrue
convertConst TFalse = PFalse
convertConst (TString s) = PString s
convertConst (TInteger i) = PInteger i

---------- Predicates -----------

isCapitalized :: String -> Bool
isCapitalized = isUpper . head

deCapitalized :: String -> String
deCapitalized (h:t) = toUpper h : t

predicates :: PTerm -> PTerm
predicates = cata (Fix . alg) where
  alg t@(PFuncApp a@(PId s) b)
    | isCapitalized s = PPredicate a b
    | otherwise       = t
  alg e = e

--------- Implication -----------

implResult :: PTerm -> PTerm
implResult = cata (Fix . alg) where
  alg (PTermBinOp PImpl a b) = PTermBinOp PImpl 
                               (addResultVar a) 
                               (addResultVar $ chainNestedFuncApp b)
  alg e = e

-- TODO: simplify with lift?
-- TODO: Find unique term instead of "Y"
addResultVar :: PTerm -> PTerm
addResultVar = unfix >>> addVar >>> Fix where
  addVar (PFuncApp f args) = PFuncApp f (args ++ [(varPTerm . PId) "Y"])

------------- chain ---------------

chainNestedFuncApp :: PTerm -> PTerm
chainNestedFuncApp = createAnd

-- | Create collection of all used variable names in the parse tree.
-- TODO: unused. Eventually use to pick unqiue names.
usedVars :: PTerm -> [String]
usedVars = cata alg where
  alg (VarPTerm (PId v)) = [v]
  alg e = concat e

-- | Transform nested function applications into 
-- | list of function application with a return variable.
createAnd :: PTerm -> PTerm
createAnd = addAnn
        >>> evalUniqNameSupplier
        >>> splitOnAnn
        >>> replaceAnn

-- Create supply monad with unique names (reader with state).
labelStream :: [String]
labelStream = fmap (("X" ++) . show) [0..]

evalUniqNameSupplier :: Supply String c -> c
evalUniqNameSupplier = flip evalSupply labelStream

-- | Annotate every function application
type PTermAnn = Cofree PTermF (Maybe String)

addAnn :: PTerm -> Supply String PTermAnn
addAnn = cataM alg where
  alg e@(PFuncApp a b) = do 
    name <- supply 
    return (Just name :< e)
  alg e = return (Nothing :< e)

test :: PTerm -> [Bool]
test = para alg where
  alg (ConstPTerm _) = [True]
  alg pterm = concatMap snd pterm

-- | Create list of parse tree at every annotation
splitOnAnn :: PTermAnn -> [PTermAnn]
splitOnAnn = para alg where
  alg :: C.CofreeF PTermF (Maybe String) (PTermAnn, [PTermAnn]) -> [PTermAnn]
  alg (m C.:< term) = case m of
      Just _  -> [m :< fmap fst term]
      Nothing -> concatMap snd term

-- | Replace child annotation in every parse tree
-- | and make it syntactically a `PAnd`.
replaceAnn :: [PTermAnn] -> PTerm
replaceAnn ts = pAnd $ fmap (cata alg) ts where
  alg (Just a C.:<    _) = (varPTerm . PId) a
  alg (_      C.:< term) = Fix term

-------------------------------------------------------------------------------
-- Print
-------------------------------------------------------------------------------

commaSep :: [Doc] -> Doc
commaSep = punctuate (text ",") >>> cat

prettyProlog :: PTerm -> Doc
prettyProlog = cata alg where
  alg (ConstPTerm c) = prettyTConst c
  alg (VarPTerm v) = prettyTVar v
  alg (TuplePTerm ts) = list ts
  alg (ListPTerm ts) = tupled ts
  alg (PTermBinOp PAddHead a b) = a <> text "#" <> b
  -- TODO: replace A
  alg (PTermBinOp PImpl a b) = a <+> text ":-" <+> b
  -- TODO: alg (PTermTerOp o a b c) = 
  alg (PAnd as) = commaSep as
  alg (PFuncApp (PId f) as) = text f <> tupled as
  alg (PPredicate (PId f) as) = (text . deCapitalized) f <> tupled as

prettyTVar :: PVar -> Doc
prettyTVar (PId s) = text $ fmap toUpper s
prettyTVar PWildcard = text "_"

prettyTConst :: PConst -> Doc
prettyTConst PTrue = text "1"
prettyTConst PFalse = text "0"
prettyTConst (PString s) = text s
prettyTConst (PInteger i) = integer i
    
-------------------------------------------------------------------------------
-- Composed
-------------------------------------------------------------------------------

prettyIsabelleInProlog :: Term -> Doc
prettyIsabelleInProlog = toPrologAST 
  >>> predicates
  >>> implResult 
  >>> prettyProlog
