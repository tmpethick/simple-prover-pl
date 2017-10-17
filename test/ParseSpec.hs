module ParseSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck
import Text.Parsec (digit)
import Parse (
  parse', xp, list, 
  Term(TermBinOp, ConstTerm, ListTerm), 
  TBinOp(TTuple),
  TConst(TTrue))

mockParseTerm = digit >> return (ConstTerm TTrue)

spec :: Spec
spec = 
  describe "parser" $ do
    context "list" $ do
      it "parses 0 element" $ do
        let Right t = parse' (list mockParseTerm) "[]"
        t `shouldBe` ListTerm []
      it "parses 1 element" $ do
          let Right t = parse' (list mockParseTerm) "[1]"
          t `shouldBe` ListTerm [ConstTerm TTrue]
      it "parses 2 element" $ do
        let Right t = parse' (list mockParseTerm) "[1,2]"
        t `shouldBe` ListTerm [ConstTerm TTrue, ConstTerm TTrue]
    context "parser" $ do
      it "tuple of lists" $ do
        let Right t = parse' xp "(True, [True, True])"
        t `shouldBe` TermBinOp TTuple 
          (ConstTerm TTrue) 
          (ListTerm [ConstTerm TTrue, ConstTerm TTrue])
      it "list of tuple" $ do
        let Right t = parse' xp "[True, (True, True)]"
        t `shouldBe` ListTerm [
          ConstTerm TTrue,  
          TermBinOp TTuple (ConstTerm TTrue) (ConstTerm TTrue)]

main :: IO ()
main = hspec spec
