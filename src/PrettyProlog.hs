module PrettyProlog where

import Data.Char
import Text.PrettyPrint.Leijen

import Parse (
  parser,
  parse',
  Term(TermBinOp, TermTerOp, TupleTerm, ConstTerm, ListTerm, VarTerm),
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

-- TODO: generalize precedence and associativity rules.
-- ppPrec :: Precedence -> Term -> Doc 
-- ppPrec p (TermBinOp TEquiv t1 t2) = addParen (p > 0) $ ppClausePrec t1 t2

  -- ppPrec _ (ConstTerm cst) = prettyTConst cst
  -- ppPrec _ (VarTerm var)   = prettyTVar var
  -- ppPrec p (ListTerm ts)   = list   $ map (ppPrec p) ts
  -- ppPrec p (TupleTerm ts)  = tupled $ map (ppPrec p) ts
  -- ppPrec p (TermTerOp TIf t1 t2 t3) = addParen (p > 5) $
  --   hsep $ merge (zipWith ppPrec [5, 6, 6] [t1, t2, t3]) 
  --                (map text ["if", "then", "else"])
  -- ppPrec p (TermBinOp TFunc t1 t2) = addParen (p > 4) $
  --   ppPrec 4 t1 <+> ppPrec 5 t2
  -- ppPrec p (TermBinOp TAddHead t1 t2) = addParen (p > 3) $
  --   ppPrec 3 t1 <+> text "#" <+> ppPrec 4 t2
  -- ppPrec p (TermBinOp TConcat t1 t2) = addParen (p > 2) $
  --   ppPrec 2 t1 <+> text "@" <+> ppPrec 3 t2  

-- ppClausePrec t1 t2 = ppHeadPrec 0 t1 <+> text ":-" <+> ppBodyPrec 0 t2

ppHeadPrec :: Integer -> Maybe Doc -> Term -> Doc
ppHeadPrec p Nothing (TermBinOp TFunc (VarTerm (TId fid)) t2) = text fid <> parens (ppHeadPrec p Nothing t2)
ppHeadPrec p (Just parent) (TermBinOp TFunc (VarTerm (TId fid)) t2) = text fid <> parens args
  where args = ppHeadPrec p Nothing t2 <> comma <> parent
ppHeadPrec p Nothing (TermBinOp TFunc t1 t2) = ppHeadPrec p (Just rhs) t1
  where rhs = ppHeadPrec p Nothing t2
ppHeadPrec p (Just parent) (TermBinOp TFunc t1 t2) = ppHeadPrec p (Just rhs) t1
  where rhs = ppHeadPrec p Nothing t2 <> comma <> parent
ppHeadPrec p parent (VarTerm tv) = prettyTVar tv

prettyTVar :: TVar -> Doc
prettyTVar (TId id) = text $ fmap toUpper id
prettyTVar Wildcard = text "_"

prettyTConst :: TConst -> Doc
prettyTConst TTrue = text "True"
prettyTConst TFalse = text "False"
prettyTConst (TString s) = text s 
prettyTConst (TInteger i) = integer i

-- prettyPrint = ppPrec 0
