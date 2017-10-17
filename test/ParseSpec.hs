module ParseSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck
import Text.Parsec (digit)
import Parse (
  parse', xp, list, 
  Term(TupleTerm, ConstTerm, ListTerm),
  TConst(TTrue))

mockParseTerm = digit >> return (ConstTerm TTrue)

spec :: Spec
spec = 
  describe "Parse" $ do
    context "list parser" $ do
      it "parses 0 element" $ do
        let Right t = parse' (list mockParseTerm) "[]"
        t `shouldBe` ListTerm []
      it "parses 1 element" $ do
          let Right t = parse' (list mockParseTerm) "[1]"
          t `shouldBe` ListTerm [ConstTerm TTrue]
      it "parses 2 element" $ do
        let Right t = parse' (list mockParseTerm) "[1,2]"
        t `shouldBe` ListTerm [ConstTerm TTrue, ConstTerm TTrue]
    context "full parser" $ do
      it "parses whitespace" $ do
        let Right t = parse' xp " True "
        t `shouldBe` ConstTerm TTrue
      it "parses inner whitespace" $ do
        let Right t = parse' xp "[ True ]"
        t `shouldBe` ListTerm [ConstTerm TTrue]
      it "parses tuple" $ do
        let Right t = parse' xp "(True,True)"
        t `shouldBe` TupleTerm [
          ConstTerm TTrue,
          ConstTerm TTrue]
      it "parses tuple of lists" $ do
        let Right t = parse' xp "(True,[True,True])"
        t `shouldBe` TupleTerm [
          ConstTerm TTrue,
          ListTerm [ConstTerm TTrue, ConstTerm TTrue]]
      it "parses list of tuple" $ do
        let Right t = parse' xp "[True,(True,True)]"
        t `shouldBe` ListTerm [
          ConstTerm TTrue,  
          TupleTerm [ConstTerm TTrue, ConstTerm TTrue]]

main :: IO ()
main = hspec spec
