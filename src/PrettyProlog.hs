{-# LANGUAGE TemplateHaskell, FlexibleContexts, DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module PrettyProlog where

import Data.Eq.Deriving (deriveEq1)
import Text.Show.Deriving (deriveShow1)
import Data.Functor.Identity (Identity)  
import Data.Char
import Control.Arrow ((>>>))
import Text.PrettyPrint.Leijen
import Control.Comonad.Cofree
import Control.Monad.Free
import Data.Functor.Foldable

import Parse (
  parser,
  parse',
  Term,
  TermF(TermBinOp, TermTerOp, TupleTerm, ConstTerm, ListTerm, VarTerm),
  TVar(TId, Wildcard),
  TConst(TTrue, TFalse, TString, TInteger),
  TBinOp(TFunc, TEquiv, TAddHead, TConcat, TConj),
  TTerOp(TIf))

----------------------------------
-- Types
----------------------------------

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

----------------------------------
-- To Prolog AST
----------------------------------

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

implResult :: PTerm -> PTerm
implResult = cata (Fix . alg) where
  -- TODO: Find unique term instead of "Y"
  alg (PTermBinOp PImpl a b) = PTermBinOp PImpl (addResultVar a) (addResultVar b)
  alg e = e

-- TODO: simplify
addResultVar = unfix >>> addVar >>> Fix where
  addVar (PFuncApp f args) = PFuncApp f (args ++ [(Fix . VarPTerm . PId) "Y"])

deCapitalized :: String -> String
deCapitalized (h:t) = toUpper h : t

isCapitalized :: String -> Bool
isCapitalized = isUpper . head

predicates :: PTerm -> PTerm
predicates = cata (Fix . alg) where
  alg t@(PFuncApp a@(PId s) b)
    | isCapitalized s = PPredicate a b
    | otherwise       = t
  alg e = e
  
-- replace PConcat and composed functions

-- type AnnAST = Cofree ASTF (Maybe String)

-- addAnn :: AST -> AnnAST
-- addAnn = cata alg where
--   alg e@(FuncApp a b) = Just "1" :< e
--   alg e = Nothing :< e


----------------------------------
-- Print
----------------------------------

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
    
----------------------------------
-- Composed
----------------------------------

prettyIsabelleInProlog :: Term -> Doc
prettyIsabelleInProlog = toPrologAST 
  >>> predicates
  >>> implResult 
  >>> prettyProlog
