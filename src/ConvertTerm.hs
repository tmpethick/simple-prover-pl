module ConvertTerm where

import Data.Functor.Foldable
import TreeDrawing
import Parse

convertTerm :: Term -> Tree String
convertTerm (Fix termf) = convertTermF termf

convertTVar :: TVar -> Tree String
convertTVar (TId name) = Node name []
convertTVar wildcard   = Node (show wildcard) []

convertTConst :: TConst -> Tree String
convertTConst TTrue          = Node "True" []
convertTConst TFalse         = Node "False" []
convertTConst (TString str)  = Node ("String " ++ str) []
convertTConst (TInteger num) = Node ("Int " ++ show num) []

-- data TUniOp = TNot deriving (Eq, Show)
convertTUniOp :: TUniOp -> Tree String
convertTUniOp term = Node (show term) []

convertTBinOp :: TBinOp -> Tree String
convertTBinOp binOp = Node (show binOp) []

-- data TTerOp = TIf deriving (Eq, Show)
convertTTerOp :: TTerOp -> Tree String
convertTTerOp term = Node ("TTerOp" ++ show term) []

-- data TQuant = TUni deriving (Eq, Show)
convertTQuant :: TQuant -> Tree String
convertTQuant term = Node ("TQuant" ++ show term) []

convertTermF :: TermF Term -> Tree String
convertTermF (ConstTerm tconst)      = Node "ConstTerm" [convertTConst tconst]
convertTermF (VarTerm tvar)          = Node "VarTerm" [convertTVar tvar]
convertTermF (TupleTerm terms)       = Node "TupleTerm" (fmap convertTerm terms)
convertTermF (ListTerm terms)        = Node "ListTerm" (fmap convertTerm terms)
convertTermF (TermUniOp _ term)      = Node "TermUniOp" [convertTerm term]
convertTermF (TermBinOp _ t1 t2)     = Node "TermBinOp" [convertTerm t1, convertTerm t2]
convertTermF (TermTerOp _ t1 t2 t3)  = Node "TermTerOp" [convertTerm t1, convertTerm t2, convertTerm t3]
convertTermF (TermQuant _ vars term) = Node "TermQuant" (fmap convertTVar vars ++ [convertTerm term])
convertTermF (TermCart terms)        = Node "TermCart" $ fmap convertTerm terms
-- convertTermF term = print $ show term
