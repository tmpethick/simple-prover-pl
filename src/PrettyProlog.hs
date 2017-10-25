{-# LANGUAGE FlexibleContexts, DeriveFunctor #-}
module PrettyProlog where

import Data.Char
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

merge :: [a] -> [a] -> [a]
merge xs     []     = xs
merge []     ys     = ys
merge (x:xs) (y:ys) = x : y : merge xs ys

addParen b p = if b then parens p else p

type Precedence = Int

-- ppClausePrec p (TermBinOp TEquiv t1 t2) = ppHeadPrec p Nothing t1 
--                         <+> text ":-" <+> ppBodyPrec p 0 t2

-- Function calls
-- ppBodyPrec p outputIdx term = text ""

-- ppHeadPrec :: Integer -> Maybe Doc -> Term -> Doc
-- ppHeadPrec p parent t = ppHeadPrec' parent (unFix t) where
--   ppHeadPrec' Nothing (TermBinOp TFunc (_ -> VarTerm (TId fid)) t2) = text fid <> parens (ppHeadPrec p Nothing (unFix t2))
--   ppHeadPrec' (Just parent) (TermBinOp TFunc (VarTerm (TId fid)) t2) = text fid <> parens args
--     where args = ppHeadPrec p Nothing t2 <> comma <> parent
--   ppHeadPrec' Nothing (TermBinOp TFunc t1 t2) = ppHeadPrec p (Just rhs) t1
--     where rhs = ppHeadPrec p Nothing t2
--   ppHeadPrec' (Just parent) (TermBinOp TFunc t1 t2) = ppHeadPrec p (Just rhs) t1
--     where rhs = ppHeadPrec p Nothing t2 <> comma <> parent
--   ppHeadPrec' parent (VarTerm tv) = prettyTVar tv

-- ppHeadPrec :: Term -> Doc

-- cata:    Nat -> Int
-- ana:     Int -> Nat
-- hylo:    ana then cata (e.g. mergeSort)
-- prepro:  natural transformation (used to filter) then cata
-- postpro: ana then natural transformation

data ASTF a = Name String
            | FuncApp a a
            deriving (Show, Eq, Functor)

type AST = Fix ASTF

name = Fix . Name
funcApp a b = Fix $ FuncApp a b

ast :: AST
ast = funcApp (funcApp (name "f") (name "a")) (name "b")

functional :: AST -> Doc
functional = cata alg where
  alg (Name s) = text s
  alg (FuncApp a b) = a <+> b

type AnnAST = Cofree ASTF (Maybe String)

addAnn :: AST -> AnnAST
addAnn = cata alg where
  alg e@(FuncApp a b) = Just "1" :< e
  alg e = Nothing :< e

-- testHisto = histo alg where
--   alg e@(FuncApp a b) = a
--   alg e = "e"
  
-- histo..
-- assumed flipped
-- FuncApp a b = a <> parens b

prettyTVar :: TVar -> Doc
prettyTVar (TId id) = text $ fmap toUpper id
prettyTVar Wildcard = text "_"

prettyTConst :: TConst -> Doc
prettyTConst TTrue = text "1"
prettyTConst TFalse = text "0"
prettyTConst (TString s) = text s
prettyTConst (TInteger i) = integer i

-- prettyPrint = ppClausePrec 0
