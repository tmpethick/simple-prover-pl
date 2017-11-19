module PrettySpec (main, spec) where

import Text.PrettyPrint.Leijen
import Test.Hspec
import Test.Hspec.Attoparsec
import Parse (parse', parser, fullParser)
import Pretty (prettyPrint)
import Control.Monad (forM_)

import TestCases (simpleProver)

-- TODO: catch left error
testPretty test = prettyPrint term `shouldBe` test
            where Right term = parse' parser test

testFullPretty test = case parse' fullParser test of 
  Right term -> prettyPrint term `shouldBe` test
  Left err   -> error $ show err

-- TODO: test precedence systematically
spec :: Spec
spec = 
  describe "Parse -> Pretty" $ do
    it "simple"       $ testPretty "[] # p"
    it "prints lists" $ testPretty "[True,True]"
    it "check p"      $ testPretty "check p \\<equiv> prover [[(0,p)]]"
    it "prover []"    $ testPretty "prover [] \\<equiv> True"
    it "prover h"     $ testPretty "prover (h # t) \\<equiv> prover (solves (h # t))"
    it "solves []"    $ testPretty "solves [] \\<equiv> []"
    it "solves h"     $ testPretty "solves (h # t) \\<equiv> solve h @ solves t"
    context "Simple Prover" $  
      forM_ simpleProver $ \syntax -> it ("test " ++ syntax) $ testFullPretty syntax

main :: IO ()
main = hspec spec
