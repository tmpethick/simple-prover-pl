{-# LANGUAGE FlexibleContexts #-}
module Parse where

import Text.Parsec
import Text.Parsec.Token as Token
import Text.Parsec.Expr as Expr
import Control.Applicative hiding ((<|>))
import Data.Functor.Identity (Identity)

parse' rule = parse rule "(source)"

-- https://wiki.haskell.org/Parsing_expressions_and_statements

-- Token based passing (lexer)

-- Pandec
-- parentases are described in Parsec Not in data type/AST.
-- leave out all before `.`. Should be represented in to AST but not used by prolog.
-- how to deal with False = True \equiv False (the infix function definition)?

-- State: Functions are also TVal in functional languages. 
-- Problem: how to capitalize only arguments and not function name?
-- Solution: Use structure. Every SeqTerm with a starting TVal must be a function. 
-- Example: (draw tree for ((a b) c)).

-- Should 0, asd, "asd" and [] be represented differently in AST?
-- Yes [] is necessary for changing representation.

-- Should @ be append on parsing?
-- Should # be add

-- Tuples: parser will strip parentases. commas can then be parsed as binary relations.
-- Problem: collision with list type..

-- Point: testing can be done by defining pretty printing. (it is the identity function then).

-- Examples:
-- prover (h # t) ≡ prover (solves (h # t))
-- "⋀p. check p ≡ prover [[(0,p)]]"
-- "⋀h t. prover (h # t) ≡ prover (solves (h # t))"
-- "prover [] ≡ True"
-- "solves [] ≡ []"
-- "⋀ h t. solves (h # t) ≡ solve h @ solves t"

-- Test cases
-- test equiv
-- test func app
-- test equiv
-- test precedence
-- test primitive
-- add support for [] (same as parens by what precedence?) [Exp] 
-- (supports [], [a#b], [a @ b] <-- just expressions inside. 
--  tricky: [a,b], since a,b is tuple.)
-- add support for And
-- add tuple ,
-- 
-- how to deal with append when this is overwriten?

data Term = 
    -- | TermUnOp TBinOp Term
      TermBinOp TBinOp Term Term
    | TermTerOp TTerOp Term Term Term
    | ConstTerm TConst
    | VarTerm TVar
    | ListTerm [Term]
  deriving (Eq, Show)
  
data TVar = TId String | Wildcard deriving (Eq, Show)

data TConst = TTrue | TFalse | TString String | TInteger Int deriving (Eq, Show)

-- Tuple implemented with comma (it is an infix operator)
data TBinOp = TFunc | TEquiv | TTuple | TAddHead | TConj deriving (Eq, Show)

data TTerOp = TIf deriving (Eq, Show)

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
  , Token.reservedNames   = ["True", "False", "if", "then", "else"]
  , Token.reservedOpNames = ["⋀" , ".", "≡", "#", "@"]
  , Token.caseSensitive   = True
  }

TokenParser { parens = m_parens
  , identifier = m_identifier
  , reservedOp = m_reservedOp
  , reserved = m_reserved
  , semiSep1 = m_semiSep1
  , whiteSpace = m_whiteSpace } = makeTokenParser langDef

xp :: ParsecT String () Identity Term
xp = buildExpressionParser table term <?> "expression"

inList :: Stream s m Char => ParsecT s u m [Term] -> ParsecT s u m Term
inList p = ListTerm <$> between (char '[') (char ']') p

sepByComma :: Stream s m Char => ParsecT s u m Term -> ParsecT s u m [Term]
sepByComma p = p `sepBy` char ','

list :: Stream s m Char => ParsecT s u m Term -> ParsecT s u m Term
list = inList . sepByComma

-- atoms
term = m_parens xp
        -- TODO: test "Truuu"
        <|> (VarTerm . TId) <$> m_identifier
        <|> list xp
        <|> pIf xp
        -- TODO: replace with `$> ?`
        <|> (m_reserved "True" >> return (ConstTerm TTrue))
        <|> (m_reserved "False" >> return (ConstTerm TFalse))

table = [ 
  -- how to do if (ternary)? https://groups.google.com/forum/#!topic/comp.lang.functional/7E2ydJLqCqs
  [Infix pFuncApp Expr.AssocLeft],
  [Infix pAdd Expr.AssocLeft],
  [Infix pTuple Expr.AssocLeft],
  [Infix pEquiv Expr.AssocLeft]
  ]

pFuncApp = m_whiteSpace >> return (TermBinOp TFunc)
pAdd = m_reservedOp "#" >> return (TermBinOp TAddHead)
pTuple = m_reservedOp "," >> return (TermBinOp TTuple)
pEquiv = m_reservedOp "≡" >> return (TermBinOp TEquiv)
-- TODO: rewrite or keep for readability?
pIf xp' = do 
  m_reserved "if"
  b <- xp'
  m_reserved "then"
  p <- xp'
  m_reserved "else"
  q <- xp'
  return (TermTerOp TIf b p q)

-- prover (h # t) ≡ prover (solves (h # t))

-- TODO: use applicative
