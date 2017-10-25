module Pretty where

import Text.PrettyPrint.Leijen
import Data.Functor.Foldable

import Parse (
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

precedence :: TermF a -> Precedence
precedence (ConstTerm cst) = 1000
precedence (VarTerm var)   = 1000
precedence (ListTerm ts)   = 1000
precedence (TupleTerm ts)  = 1000
precedence (TermTerOp TIf _ _ _) = 900
precedence (TermBinOp TFunc _ _) = 800
precedence (TermBinOp TAddHead _ _) = 700
precedence (TermBinOp TConcat _ _) = 600
precedence (TermBinOp TConj _ _) = 500
precedence (TermBinOp TEquiv _ _) = 400

-- TODO: generalize precedence and associativity rules.
prettyPrintPrec :: Term -> Doc 
prettyPrintPrec = cata alg where
  alg (ConstTerm cst) = prettyTConst cst
  alg (VarTerm var)   = prettyTVar var
  alg (ListTerm ts)   = list ts
  alg (TupleTerm ts)  = tupled ts
  alg (TermTerOp TIf t1 t2 t3) = 
    hsep $ merge [t1, t2, t3] (map text ["if", "then", "else"])
  alg (TermBinOp TFunc t1 t2) = t1 <+> t2
  alg (TermBinOp TAddHead t1 t2) = t1 <+> text "#" <+> t2
  alg (TermBinOp TConcat t1 t2) = t1 <+> text "@" <+> t2
  alg (TermBinOp TConj t1 t2) = t1 <+> text "⋀" <+> t2
  alg (TermBinOp TEquiv t1 t2) = t1 <+> text "\\<equiv>" <+> t2

prettyTVar :: TVar -> Doc
prettyTVar (TId id) = text id
prettyTVar Wildcard = text "_"

prettyTConst :: TConst -> Doc
prettyTConst TTrue = text "True"
prettyTConst TFalse = text "False"
prettyTConst (TString s) = text s 
prettyTConst (TInteger i) = integer i

-- Hack: Explicit state width of 1000 to avoid newlines.
-- Default would otherwise be `show $ prettyPrintPrec 0 term`.
prettyPrint term = (displayS . renderPretty 1 1000 . prettyPrintPrec) term ""
