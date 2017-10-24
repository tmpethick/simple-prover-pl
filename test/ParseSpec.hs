module ParseSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck
import Text.Parsec (digit)
import Parse (
  parse', parser, list, 
  constTerm, varTerm, tupleTerm, listTerm, termBinOp, termTerOp,
  TermF(TupleTerm, ConstTerm, ListTerm),
  TConst(TTrue))

mockParseTerm = digit >> return (constTerm TTrue)

spec :: Spec
spec = 
  describe "Parse" $ do
    context "list parser" $ do
      it "parses 0 element" $ do
        let Right t = parse' (list mockParseTerm) "[]"
        t `shouldBe` listTerm []
      it "parses 1 element" $ do
          let Right t = parse' (list mockParseTerm) "[1]"
          t `shouldBe` listTerm [constTerm TTrue]
      it "parses 2 element" $ do
        let Right t = parse' (list mockParseTerm) "[1,2]"
        t `shouldBe` listTerm [constTerm TTrue, constTerm TTrue]
    context "full parser" $ do
      it "parses whitespace" $ do
        let Right t = parse' parser " True "
        t `shouldBe` constTerm TTrue
      it "parses inner whitespace" $ do
        let Right t = parse' parser "[ True ]"
        t `shouldBe` listTerm [constTerm TTrue]
      it "parses parents" $ do
        let Right t = parse' parser "(True)"
        t `shouldBe` constTerm TTrue
      it "parses tuple" $ do
        let Right t = parse' parser "(True,True)"
        t `shouldBe` tupleTerm [
          constTerm TTrue,
          constTerm TTrue]
      it "parses tuple of lists" $ do
        let Right t = parse' parser "(True,[True,True])"
        t `shouldBe` tupleTerm [
          constTerm TTrue,
          listTerm [constTerm TTrue, constTerm TTrue]]
      it "parses list of tuple" $ do
        let Right t = parse' parser "[True,(True,True)]"
        t `shouldBe` listTerm [
          constTerm TTrue,  
          tupleTerm [constTerm TTrue, constTerm TTrue]]

main :: IO ()
main = hspec spec
