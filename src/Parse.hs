{-# LANGUAGE TemplateHaskell, FlexibleContexts, DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module Parse where

import Data.Functor.Foldable
import Text.Parsec
import Data.Eq.Deriving (deriveEq1)
import Text.Show.Deriving (deriveShow1)
import Text.Parsec.Token as Token
import Text.Parsec.Expr as Expr
import Control.Applicative hiding ((<|>), many)
import Data.Functor.Identity (Identity)

parse' rule = parse rule "(source)"

data TVar = TId String | Wildcard deriving (Eq, Show)

data TConst = 
    TTrue 
  | TFalse 
  | TString String 
  | TInteger Integer 
  deriving (Eq, Show)

data TBinOp = 
    TFunc 
    | TAddHead 
    | TConcat 
    | TConj 
    | TEquiv 
  deriving (Eq, Show)

data TTerOp = TIf deriving (Eq, Show)

data TermF a = 
  ConstTerm TConst
  | VarTerm TVar
  | TupleTerm [a]
  | ListTerm [a]
  | TermBinOp TBinOp a a
  | TermTerOp TTerOp a a a
  deriving (Show, Eq, Functor, Foldable, Traversable)

deriveShow1 ''TermF
deriveEq1 ''TermF

type Term = Fix TermF

constTerm :: TConst -> Term
constTerm = Fix . ConstTerm

varTerm :: TVar -> Term
varTerm = Fix . VarTerm

tupleTerm :: [Term] -> Term
tupleTerm = Fix . TupleTerm

listTerm :: [Term] -> Term
listTerm = Fix . ListTerm

termBinOp :: TBinOp -> Term -> Term -> Term
termBinOp o a b = Fix $ TermBinOp o a b

termTerOp :: TTerOp -> Term -> Term -> Term -> Term
termTerOp o a b c = Fix $ TermTerOp o a b c

langDef :: Token.LanguageDef ()
langDef = Token.LanguageDef
  { Token.commentStart    = ""
  , Token.commentEnd      = ""
  , Token.commentLine     = ""
  , Token.nestedComments  = False
  , Token.identStart      = letter
  , Token.identLetter     = alphaNum <|> oneOf "_'"
  , Token.opStart         = Token.opLetter langDef
  , Token.opLetter        = oneOf ",:!#$%&*+./<=>?@\\^|-~"
  , Token.reservedNames   = ["True", "False"]
  , Token.reservedOpNames = ["\\<equiv>", "#", "@", "if", "then", "else"]
  , Token.caseSensitive   = True
  }

TokenParser { parens = m_parens
  , identifier = m_identifier
  , reservedOp = m_reservedOp
  , reserved = m_reserved
  , symbol = m_symbol
  , natural = m_natural
  , whiteSpace = m_whiteSpace } = makeTokenParser langDef

type StringParser = ParsecT String () Identity

sepByComma :: StringParser Term -> StringParser [Term]
sepByComma p = p `sepBy` m_symbol ","

sepByComma2 :: StringParser Term -> StringParser [Term]
sepByComma2 p = do
  x1 <- p
  m_symbol ","
  x2 <- p
  xs <- many (m_symbol "," >> p)
  return (x1:x2:xs)

inTuple :: StringParser [Term] -> StringParser Term
inTuple p = tupleTerm <$> between (m_symbol "(") (m_symbol ")") p

inList :: StringParser [Term] -> StringParser Term
inList p = listTerm <$> between (m_symbol "[") (m_symbol "]") p

list :: StringParser Term -> StringParser Term
list p = (inList . sepByComma) p <?> "list"

tuple :: StringParser Term -> StringParser Term
tuple p = (inTuple . sepByComma2) p <?> "tuple"

xp :: StringParser Term
xp = buildExpressionParser table term <?> "expression"
-- TODO: move whiteSpace to mainParser
-- mainParser = m_whiteSpace >> xp

-- Use `try` since `m_parens` and `tuple` shares the same starting char.
-- TODO: using `try` might not be desirable.. (research why)
term = try (m_parens xp)
  <|> tuple xp 
  <|> list xp
  <|> (m_reserved "True"     >> return (constTerm TTrue))
  <|> (m_reserved "False"    >> return (constTerm TFalse))
  <|> (constTerm . TInteger) <$> m_natural
  <|> (varTerm . TId)        <$> m_identifier
  <|> pIf xp

table = [ 
    [Infix pFuncApp Expr.AssocLeft],
    [Infix pConcat Expr.AssocLeft ],
    [Infix pAdd Expr.AssocLeft    ],
    [Infix pEquiv Expr.AssocLeft  ]
    ]
    
-- Function application is just whitespace.. 
pFuncApp = return (termBinOp TFunc)

pAdd    = m_reservedOp "#"         >> return (termBinOp TAddHead)
pConcat = m_reservedOp "@"         >> return (termBinOp TConcat)
pEquiv  = m_reservedOp "\\<equiv>" >> return (termBinOp TEquiv)

-- TODO: rewrite or keep for readability?
pIf expr = do 
  m_reserved "if"
  b <- expr
  m_reserved "then"
  p <- expr
  m_reserved "else"
  q <- expr
  return (termTerOp TIf b p q)

parser = m_whiteSpace >> xp <* eof
