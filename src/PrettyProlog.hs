{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}

module PrettyProlog where

import qualified Data.List as List
import Data.Eq.Deriving (deriveEq1)
import Text.Show.Deriving (deriveShow1)
import Data.Functor.Identity (Identity)  
import Data.Char
import qualified Data.Foldable as F
import Control.Arrow ((>>>), (&&&))
import Text.PrettyPrint.Leijen
import qualified Control.Comonad.Trans.Cofree as C
import Control.Comonad.Cofree (Cofree((:<)))
import Control.Monad.Free
import Data.Functor.Foldable
import Control.Monad.Supply (Supply, supply, evalSupply)
import qualified Data.Set as Set
import Data.Deriving (deriveOrd1)

import Foldable (cataM)
import Parse (
  Term,
  TermF(TermUniOp, TermBinOp, TermTerOp,
    TupleTerm, ConstTerm, ListTerm, VarTerm,
    TermQuant, TermCart),
  TUniOp(TNot),
  TVar(TId, Wildcard),
  TConst(TTrue, TFalse, TString, TInteger),
  TBinOp(TFunc, TEquiv, TAddHead, TConcat, TConj, TEq),
  TTerOp(TIf))

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

data PVar = PId String | PWildcard deriving (Eq, Show, Ord)

data PConst = PTrue | PFalse | PString String | PInteger Integer deriving (Eq, Show, Ord)

data PBinOp = PAddHead | PRule deriving (Eq, Show, Ord)

data PTerOp = PIf deriving (Eq, Show, Ord)

data PTermF a = 
    ConstPTerm PConst
  | VarPTerm PVar
  | TuplePTerm [a]
  | ListPTerm [a]
  | PTermBinOp PBinOp a a
  | PTermTerOp PTerOp a a a
  | PAnd [a]
  | PFuncApp PVar [a]
  | PFact PVar [a]
  | PPredicate PVar [a]
  | PTerms [a]
  deriving (Show, Eq, Functor, Foldable, Traversable, Ord)

deriveShow1 ''PTermF
deriveEq1 ''PTermF
deriveOrd1 ''PTermF

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

pFact :: PVar -> [PTerm] -> PTerm
pFact v a = Fix $ PFact v a

pFuncApp :: PVar -> [PTerm] -> PTerm
pFuncApp v a = Fix $ PFuncApp v a

pPredicate :: PVar -> [PTerm] -> PTerm
pPredicate v a = Fix $ PPredicate v a

pTerms :: [PTerm] -> PTerm
pTerms as = Fix $ PTerms as

-------------------------------------------------------------------------------
-- To Prolog AST
-------------------------------------------------------------------------------

-------- AST convertion ----------

toPrologAST :: Term -> PTerm
toPrologAST = cata alg where
  -- Convert nested one arg function applications
  alg (TermBinOp TFunc a b) = case unfix a of 
    (VarPTerm f)    -> pFuncApp f [b]
    (PFuncApp f b') -> pFuncApp f (b' ++ [b])
  
  -- The following are just "function calls" (predicates).
  alg (TermBinOp TConcat a b) = pFuncApp (PId "append") [a, b]
  alg (TermBinOp TEq a b)     = pFuncApp (PId "eq") [a, b]
  alg (TermBinOp TConj a b)   = pFuncApp (PId "conj") [a, b]
  alg (TermUniOp TNot a)      = pFuncApp (PId "not") [a]
  alg (TermTerOp TIf a b c)   = pFuncApp (PId "ifelse") [a, b, c]

  -- 1-to-1 translations
  alg (ConstTerm c)       = constPTerm (convertConst c)
  alg (VarTerm v)         = varPTerm (convertVar v)
  alg (TupleTerm as)      = tuplePTerm as
  alg (ListTerm as)       = listPTerm as
  alg (TermBinOp o a b)   = pTermBinOp (convertBinOp o) a b
  -- alg (TermTerOp o a b c) = pTermTerOp (convertTerOp o) a b c
  alg (TermQuant _ _ a)   = a
  alg (TermCart as)       = pTerms as

convertBinOp TAddHead = PAddHead
convertBinOp TEquiv   = PRule

convertTerOp TIf = PIf

convertVar (TId id) = PId id
convertVar Wildcard = PWildcard
  
convertConst TTrue        = PTrue
convertConst TFalse       = PFalse
convertConst (TString s)  = PString s
convertConst (TInteger i) = PInteger i

---------- Predicates -----------

isCapitalized :: String -> Bool
isCapitalized = isUpper . head

deCapitalized :: String -> String
deCapitalized (h:t) = toLower h : t

predicates :: PTerm -> PTerm
predicates = cata (Fix . alg) where
  alg t@(PFuncApp a@(PId s) b)
    | isCapitalized s = PPredicate a b
    | otherwise       = t
  alg e = e

--------- Implication -----------

-- TODO: based Y on last in PAnd.
implResult :: PTerm -> PTerm
implResult = cata alg where
  alg (PTermBinOp PRule a b) = 
    let 
      (depTerms, bodyRoot) = chainNestedFuncApp b
      head      = case unfix bodyRoot of
        (PFuncApp f args) -> addResultVar a
        _                 -> addArg bodyRoot a
      bodyTerms = case unfix bodyRoot of
        (PFuncApp f args) -> depTerms ++ [addResultVar bodyRoot]
        _                 -> depTerms
    in case bodyTerms of
      [] -> convertToFact head
      _  -> pTermBinOp PRule head (pAnd bodyTerms)
  alg e = Fix e

convertToFact = liftFix (\(PFuncApp f args) -> PFact f args)

-- TODO: simplify unfix/fix with lift analog?
addArg :: PTerm -> PTerm -> PTerm
addArg arg = liftFix (\(PFuncApp f args) -> PFuncApp f (args ++ [arg]))

-- TODO: Find unique term instead of "Y"
addResultVar :: PTerm -> PTerm
addResultVar = addArg $ (varPTerm . PId) "Y"

------------- chain ---------------

-- | Create collection of all used variable names in the parse tree.
usedVars :: PTerm -> [String]
usedVars = cata alg where
  alg (VarPTerm (PId v)) = [v]
  alg e = concat e

-- | Transform nested function applications into 
-- | list of function application with a return variable.
chainNestedFuncApp :: PTerm -> ([PTerm], PTerm)
chainNestedFuncApp = addAnn
        >>> evalUniqNameSupplier
        >>> removeRootAnn
        >>> ((splitOnAnn >>> applyRootAnns >>> replaceAnns) &&& replaceAnn)

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

-- | Create list of parse tree at every annotation
splitOnAnn :: PTermAnn -> [PTermAnn]
splitOnAnn = para alg where
  alg :: C.CofreeF PTermF (Maybe String) (PTermAnn, [PTermAnn]) -> [PTermAnn]
  alg (m C.:< term) = case m of
      Just _  -> F.fold (fmap snd term) ++ [m :< fmap fst term]
      Nothing -> F.fold (fmap snd term)

-- TODO: Somehow consolidate addVar and addVarAnn. 
--       Problem is that there is no equivalent of `return` for Fix and Cofree.
--       The VarTerm need to be wrapped in a Fix/Cofree.
addVarAnn var (PFuncApp f args) = PFuncApp f (args ++ [Nothing :< (VarPTerm . PId) var])

-- TODO: rewrite to using Cofree functions.
removeRootAnn :: PTermAnn -> PTermAnn
removeRootAnn (_ :< t) = Nothing :< t

moveRootAnnToArg :: PTermAnn -> PTermAnn
moveRootAnnToArg (Just m :< t) = Nothing :< addVarAnn m t

-- | Append return var to func args and `replaceAnn` do not replace the roots with VarTerm.
-- | Also do not add to args for the last term since `implResult` does this.
applyRootAnns :: [PTermAnn] -> [PTermAnn]
applyRootAnns = fmap moveRootAnnToArg

-- | Replace child annotation in every parse tree
replaceAnn :: PTermAnn -> PTerm
replaceAnn = cata alg where
  alg (Just a C.:<    _) = (varPTerm . PId) a
  alg (_      C.:< term) = Fix term

replaceAnns :: [PTermAnn] -> [PTerm]
replaceAnns = fmap replaceAnn

-------------------------------------------------------------------------------
-- Print
-------------------------------------------------------------------------------

commaSep :: [Doc] -> Doc
commaSep = punctuate (text ", ") >>> cat

prettyProlog :: PTerm -> Doc
prettyProlog = cata alg where
  alg (ConstPTerm c)            = prettyTConst c
  alg (VarPTerm v)              = prettyTVar v
  alg (TuplePTerm ts)           = tupled ts
  alg (ListPTerm ts)            = list ts
  alg (PTermBinOp PAddHead a b) = list [a <> text "|" <> b]
  -- TODO: replace A
  alg (PTermBinOp PRule a b)    = a <+> text ":-" <+> b <> text "."
  alg (PFact (PId f) as)        = text f <> tupled as <> text "."
  alg (PTermTerOp PIf a b c)    = a <+> text "->" <+> b <+> text ";" <+> c
  alg (PAnd as)                 = commaSep as
  alg (PFuncApp (PId f) as)     = text f <> tupled as
  alg (PPredicate (PId f) as)   = (text . deCapitalized) f <> tupled as
  alg (PTerms as)               = sepLine as

sepLine = cat . punctuate line

replace a b c = if c == a then b else c

prettyTVar :: PVar -> Doc
prettyTVar (PId s) = text $ fmap (toUpper . replace '\'' '_') s
prettyTVar PWildcard = text "_"

prettyTConst :: PConst -> Doc
prettyTConst PTrue = text "1"
prettyTConst PFalse = text "0"
prettyTConst (PString s) = text s
prettyTConst (PInteger i) = integer i

countOccurances :: [String] -> Set.Set String
countOccurances = List.sort >>> List.group -- create groups of occurances
             >>> map (head &&& length)     -- count occurances
             >>> filter ((1==) . snd)      -- only select 1 occurances
             >>> map fst                   -- discard count
             >>> Set.fromList              -- make lookup effecient

varCount :: PTerm -> Set.Set String
varCount term = countOccurances $ usedVars term

removeSingletonVars :: PTerm -> PTerm
removeSingletonVars term = cata (Fix . alg) term where 
  alg e@(VarPTerm (PId v)) = if isSingleton v then VarPTerm PWildcard else e
  alg e = e
  isSingleton v = Set.member v singletons
  singletons = varCount term 

liftFix :: (PTermF PTerm -> PTermF PTerm) -> PTerm -> PTerm
liftFix f = unfix >>> f >>> Fix

removeSingletonVarsFromTerms :: PTerm -> PTerm
removeSingletonVarsFromTerms = liftFix (\(PTerms ts) -> PTerms $ fmap removeSingletonVars ts) 

sortTerms :: PTerm -> PTerm
sortTerms = liftFix (\(PTerms ts) -> PTerms $ List.sort ts)

-------------------------------------------------------------------------------
-- Composed
-------------------------------------------------------------------------------

isabelleToProlog :: Term -> PTerm
isabelleToProlog = toPrologAST 
               >>> predicates
               >>> implResult 
               >>> removeSingletonVarsFromTerms
               -- >>> sortTerms

prettyIsabelleInProlog :: Term -> Doc
prettyIsabelleInProlog = isabelleToProlog >>> prettyProlog
