module PrettyPrologSpec (main, spec) where
  
  import Text.PrettyPrint.Leijen
    
  import Test.Hspec
  import Test.Hspec.Attoparsec
  import Control.Arrow ((>>>))
  import Parse (StringParser, Term, parse', parser, fullParser)
  import Pretty (docToString)
  import Control.Comonad.Cofree  
  import PrettyProlog (
    addAnn, evalUniqNameSupplier, splitOnAnn,
    toPrologAST, predicates, 
    prettyIsabelleInProlog, isabelleToProlog, prettyProlog,
    constPTerm, varPTerm, tuplePTerm, listPTerm, 
    pTermBinOp, pTermTerOp, pAnd, pFuncApp, pPredicate,
    PTerm,
    PTermF(ConstPTerm, PFuncApp, VarPTerm),
    PConst(PTrue, PFalse),
    PVar(PId))
  import Control.Monad (forM_)
    
  import TestCases (simpleProverPair)

  testProlog :: (Term -> PTerm) -> String -> PTerm -> Expectation
  testProlog f test expected = case parse' parser test of 
    Right isabelle -> f isabelle `shouldBe` expected
    Left err       -> error $ show err

  toPrologPredicates :: Term -> PTerm
  toPrologPredicates = toPrologAST >>> predicates
  
  toProlog :: StringParser Term -> String -> PTerm
  toProlog parser = parse' parser >>> justRight >>> isabelleToProlog where
    justRight (Right ast) = ast
    justRight (Left err)  = error $ show err

  translate :: StringParser Term -> String -> String
  translate parser = toProlog parser >>> prettyProlog >>> docToString

  translateExpr = translate parser
  translateFull = translate fullParser

  spec :: Spec
  spec = 
    describe "PrettyProlog" $ do
      context "toPrologAST" $
        it "simple prolog conversion" $ 
          testProlog toPrologAST "[True,True]" (listPTerm [constPTerm PTrue, constPTerm PTrue])
      context "predicates" $
        it "f(a,b,c)" $
          testProlog toPrologPredicates "P a b" (pPredicate (PId "P") [varPTerm $ PId "a", varPTerm $ PId "b"])
      -- TODO: replace with more focused testing and leave e2e to a separate test.
      context "prettyIsabelleInProlog" $ do
          it "prints lists" $ translateExpr "[True,True]"
                                 `shouldBe` "[1,1]"
          it "check p"      $ translateExpr "check p \\<equiv> prover [[(0,p)]]"
                                 `shouldBe` "check(P,Y) :- prover([[(0,P)]],Y)."
          it "prover []"    $ translateExpr "prover [] \\<equiv> True"
                                 `shouldBe` "prover([],1)."
          it "prover h"     $ translateExpr "prover (h # t) \\<equiv> prover (solves (h # t))"
                                 `shouldBe` "prover([H|T],Y) :- solves([H|T],X0), prover(X0,Y)."
          it "solves []"    $ translateExpr "solves [] \\<equiv> []"
                                 `shouldBe` "solves([],[])."
          it "solves h"     $ translateExpr "solves (h # t) \\<equiv> solve h @ solves t"
                                 `shouldBe` "solves([H|T],Y) :- solve(H,X0), solves(T,X1), append(X0,X1,Y)."
      context "annotation" $
        it "should split nested func app" $
          let annotate = splitOnAnn . evalUniqNameSupplier . addAnn
          in annotate (toProlog parser "prover(solve(a))") `shouldBe` 
          [Just "X0" :< PFuncApp (PId "solve")  [Nothing :< VarPTerm (PId "a")],
           Just "X1" :< PFuncApp (PId "prover") [Just "X0" :< PFuncApp (PId "solve") 
                                                [Nothing :< VarPTerm (PId "a")]]]
      context "Simple Prover" $  
        forM_ [simpleProverPair !! 8] $ \(isabelle, prolog) -> 
          it ("test " ++ isabelle) $ translateFull isabelle `shouldBe` prolog

  main :: IO ()
  main = hspec spec
  