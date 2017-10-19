module Pretty where

import Text.PrettyPrint.Leijen

import Parse (
  Term(TermBinOp, TermTerOp, TupleTerm, ConstTerm, ListTerm, VarTerm),
  TVar(TId, Wildcard),
  TConst(TTrue, TFalse, TString, TInteger),
  TBinOp(TFunc, TEquiv, TAddHead, TConcat, TConj),
  TTerOp(TIf))

merge :: [a] -> [a] -> [a]
merge xs     []     = xs
merge []     ys     = ys
merge (x:xs) (y:ys) = x : y : merge xs ys
  
prettyPrint :: Term -> Doc
prettyPrint (TermBinOp TFunc t1 t2) = prettyPrint t1 <+> prettyPrint t2
prettyPrint (TermBinOp TEquiv t1 t2) = prettyPrint t1 <+> text "≡" <+> prettyPrint t2
prettyPrint (TermBinOp TConcat t1 t2) = prettyPrint t1 <+> text "@" <+> prettyPrint t2
prettyPrint (TermBinOp TAddHead t1 t2) = prettyPrint t1 <+> text "#" <+> prettyPrint t2
prettyPrint (TermBinOp TConj t1 t2) = prettyPrint t1 <+> text "⋀" <+> prettyPrint t2
prettyPrint (TermTerOp TIf t1 t2 t3) = hsep $ merge (map prettyPrint [t1, t2, t3]) 
                                                    (map text ["if", "then", "else"])
prettyPrint (ConstTerm cst) = prettyTConst cst
prettyPrint (VarTerm var) = prettyTVar var
prettyPrint (ListTerm ts) = list $ map prettyPrint ts
prettyPrint (TupleTerm ts) = tupled $ map prettyPrint ts

prettyTVar :: TVar -> Doc
prettyTVar (TId id) = text id
prettyTVar Wildcard = text "_"

prettyTConst :: TConst -> Doc
prettyTConst TTrue = text "True"
prettyTConst TFalse = text "False"
prettyTConst (TString s) = text s 
prettyTConst (TInteger i) = integer i
