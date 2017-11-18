module Pretty where

import Text.PrettyPrint.Leijen
import Data.Functor.Foldable

import Parse (
  Term,
  TermF(TermUniOp, TermBinOp, TermTerOp, 
        TupleTerm, ConstTerm, ListTerm, 
        VarTerm, TermCart, TermQuant),
  TVar(TId, Wildcard),
  TConst(TTrue, TFalse, TString, TInteger),
  TBinOp(TFunc, TEquiv, TAddHead, TConcat, TEq, TConj),
  TUniOp(TNot),
  TQuant(TUni),
  TTerOp(TIf))

merge :: [a] -> [a] -> [a]
merge xs     []     = xs
merge []     ys     = ys
merge (x:xs) (y:ys) = x : y : merge xs ys

addParen b p = if b then parens p else p

type Precedence = Int

precedence :: TermF a -> Precedence
precedence (ConstTerm cst)          = 1000
precedence (VarTerm var)            = 1000
precedence (ListTerm ts)            = 1000
precedence (TupleTerm ts)           = 1000
precedence (TermBinOp TFunc _ _)    = 900
precedence (TermUniOp TNot _)       = 850
precedence (TermBinOp TConcat _ _)  = 800
precedence (TermBinOp TAddHead _ _) = 700
precedence (TermBinOp TEq _ _)      = 600
precedence (TermBinOp TConj _ _)    = 500
precedence (TermBinOp TEquiv _ _)   = 400
precedence (TermTerOp TIf _ _ _)    = 300
precedence TermQuant{}              = 200
precedence TermCart{}               = 100

withParen :: TermF a -> Precedence -> (Term, Doc) -> Doc
withParen t1 p (t, d) = addParen (precedence (unfix t) < precedence t1 + p) d

-- TODO: generalize precedence and associativity rules.
prettyPrintPrec :: Term -> Doc 
prettyPrintPrec = para alg where
  alg t = alg' t where
    withParen' = withParen t
    withParen0 = withParen' 0
    alg' (ConstTerm cst) = prettyTConst cst
    alg' (VarTerm var)   = prettyTVar var
    alg' (ListTerm ts)   = list $ fmap snd ts
    alg' (TupleTerm ts)  = tupled $ fmap snd ts
    alg' (TermTerOp TIf t1 t2 t3) = 
      hsep $ merge (map text ["if", "then", "else"])
                   (zipWith withParen' [0, 0, 1] [t1, t2, t3]) 
    alg' (TermUniOp TNot t1)      = text "\\<not>" <+> withParen0 t1
    alg' (TermBinOp TFunc t1 t2) = withParen0 t1 <+> withParen' 1 t2
    alg' (TermBinOp op t1 t2)    = withParen0 t1 <+> sep op <+> withParen' 1 t2 where 
      sep TAddHead = text "#"
      sep TConcat  = text "@"
      sep TEq      = text "="
      sep TConj    = text "\\<and>"
      sep TEquiv   = text "\\<equiv>"
    alg' (TermQuant TUni vs e) = text "\\<And>" <> sep (fmap prettyTVar vs) <> text "." <+> withParen0 e
    alg' (TermCart ts) = sepLine $ fmap (wrapInCartouche . withParen0) ts

wrapInCartouche t = text "\\<open>" <> t <> text "\\<close>"
sepLine = cat . punctuate line

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
docToString :: Doc -> String
docToString doc = (displayS . renderPretty 1 1000) doc ""

prettyPrint :: Term -> String
prettyPrint = docToString . prettyPrintPrec
