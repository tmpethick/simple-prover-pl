module PrettySpec (main, spec) where

import Text.PrettyPrint.Leijen
  
import Test.Hspec
import Test.Hspec.Attoparsec
import Parse (parse', parser)
import Pretty (prettyPrint)

-- TODO: catch left error
testPretty test = prettyPrint term `shouldBe` test
            where Right term = parse' parser test

-- TODO: test precedence systematically
spec :: Spec
spec = 
  describe "Parse -> Pretty" $ do
    it "simple" $ testPretty "[] # p"
    it "prints lists" $ testPretty "[True,True]"
    it "check p" $ testPretty "check p \\<equiv> prover [[(0,p)]]"
    it "prover []" $ testPretty "prover [] \\<equiv> True"
    it "prover h" $ testPretty "prover (h # t) \\<equiv> prover (solves (h # t))"
    it "solves []" $ testPretty "solves [] \\<equiv> []"
    it "solves h" $ testPretty "solves (h # t) \\<equiv> solve h @ solves t"

main :: IO ()
main = hspec spec
