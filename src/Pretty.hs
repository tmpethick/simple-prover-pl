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

-- TODO: generalize precedence and associativity rules.
prettyPrintPrec :: Precedence -> Term -> Doc 
prettyPrintPrec p t = prettyPrintPrec' (unfix t) where
  prettyPrintPrec' (ConstTerm cst) = prettyTConst cst
  prettyPrintPrec' (VarTerm var)   = prettyTVar var
  prettyPrintPrec' (ListTerm ts)   = list   $ map (prettyPrintPrec p) ts
  prettyPrintPrec' (TupleTerm ts)  = tupled $ map (prettyPrintPrec p) ts
  prettyPrintPrec' (TermTerOp TIf t1 t2 t3) = addParen (p > 5) $
    hsep $ merge (zipWith prettyPrintPrec [5, 6, 6] [t1, t2, t3]) 
                (map text ["if", "then", "else"])
  prettyPrintPrec' (TermBinOp TFunc t1 t2) = addParen (p > 4) $
    prettyPrintPrec 4 t1 <+> prettyPrintPrec 5 t2
  prettyPrintPrec' (TermBinOp TAddHead t1 t2) = addParen (p > 3) $
    prettyPrintPrec 3 t1 <+> text "#" <+> prettyPrintPrec 4 t2
  prettyPrintPrec' (TermBinOp TConcat t1 t2) = addParen (p > 2) $
    prettyPrintPrec 2 t1 <+> text "@" <+> prettyPrintPrec 3 t2
  prettyPrintPrec' (TermBinOp TConj t1 t2) = addParen (p > 1) $
    prettyPrintPrec 1 t1 <+> text "⋀" <+> prettyPrintPrec 2 t2
  prettyPrintPrec' (TermBinOp TEquiv t1 t2) = addParen (p > 0) $
    prettyPrintPrec 0 t1 <+> text "\\<equiv>" <+> prettyPrintPrec 1 t2

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
prettyPrint term = (displayS . renderPretty 1 1000 . prettyPrintPrec 0) term ""
