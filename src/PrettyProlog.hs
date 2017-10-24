module PrettyProlog where

import Data.Char
import Text.PrettyPrint.Leijen

import Data.Fix
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
-- prepro:  

prettyTVar :: TVar -> Doc
prettyTVar (TId id) = text $ fmap toUpper id
prettyTVar Wildcard = text "_"

prettyTConst :: TConst -> Doc
prettyTConst TTrue = text "True"
prettyTConst TFalse = text "False"
prettyTConst (TString s) = text s 
prettyTConst (TInteger i) = integer i

-- prettyPrint = ppClausePrec 0
