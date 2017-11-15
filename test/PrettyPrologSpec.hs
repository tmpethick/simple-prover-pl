module PrettyPrologSpec (main, spec) where
  
  import Text.PrettyPrint.Leijen
    
  import Test.Hspec
  import Test.Hspec.Attoparsec
  import Control.Arrow ((>>>))
  import Parse (Term, parse', parser)
  import PrettyProlog (toPrologAST, predicates, 
    constPTerm, varPTerm, tuplePTerm, listPTerm, 
    pTermBinOp, pTermTerOp, pAnd, pFuncApp, pPredicate,
    PTerm,
    PConst(PTrue, PFalse),
    PVar(PId))
  
  -- -- TODO: catch left error
  -- testProlog test expected = show (ppHeadPrec 0 Nothing term) `shouldBe` expected
  --             where Right term = parse' parser test

  testProlog :: (Term -> PTerm) -> String -> PTerm -> Expectation
  testProlog f test expected = val `shouldBe` expected
            where 
              Right isabelle = parse' parser test
              val = f isabelle

  toPrologPredicates = toPrologAST >>> predicates

  spec :: Spec
  spec = 
    describe "PrettyProlog" $ do
      context "toPrologAST" $
        it "simple prolog conversion" $ 
          testProlog toPrologAST "[True,True]" (listPTerm [constPTerm PTrue, constPTerm PTrue])
      context "predicates" $
        it "f(a,b,c)" $
          testProlog toPrologPredicates "P a b" (pPredicate (PId "P") [varPTerm $ PId "a", varPTerm $ PId "b"])

  --     it "3" $ testProlog "f a1 a2 a3" "f(A1,A2,A3)"
  --     it "2" $ testProlog "f a1 a2" "f(A1,A2)"
  --     it "1" $ testProlog "f a1" "f(A1)"
  
  main :: IO ()
  main = hspec spec
  