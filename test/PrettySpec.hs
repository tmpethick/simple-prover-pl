module PrettySpec (main, spec) where

import Text.PrettyPrint.Leijen
  
import Test.Hspec
import Test.Hspec.Attoparsec
import Parse (parse', parser)
import Pretty (prettyPrint)

-- TODO: catch left error
testPretty test = show (prettyPrint term) `shouldBe` test
            where Right term = parse' parser test

spec :: Spec
spec = 
  describe "Parse -> Pretty" $ do
    it "simple" $ testPretty "[] # p"
    -- it "prints lists" $ testPretty "[True,True]"
    -- it "check p" $ testPretty "check p ≡ prover [[(0,p)]]"
    -- it "prover []" $ testPretty "prover [] ≡ True"
    -- it "prover h" $ testPretty "prover (h # t) ≡ prover (solves (h # t))"
    -- it "solves []" $ testPretty "solves [] ≡ []"
    -- it "solves h" $ testPretty "solves (h # t) ≡ solve h @ solves t"

main :: IO ()
main = hspec spec
