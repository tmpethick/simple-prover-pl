module PrettyPrologSpec (main, spec) where
  
  import Text.PrettyPrint.Leijen
    
  import Test.Hspec
  import Test.Hspec.Attoparsec
  -- import Parse (parse', parser)
  -- import PrettyProlog (ppHeadPrec)
  
  -- -- TODO: catch left error
  -- testProlog test expected = show (ppHeadPrec 0 Nothing term) `shouldBe` expected
  --             where Right term = parse' parser test
  
  spec :: Spec
  spec = 
    describe "Parse -> Prolog" $ it "test" $ "" `shouldBe` ""
  --     it "23" $ testProlog "f a1 (g a2 a3) a4" "f(A1,g(A2,A3),A4)"
  --     it "3" $ testProlog "f a1 a2 a3" "f(A1,A2,A3)"
  --     it "2" $ testProlog "f a1 a2" "f(A1,A2)"
  --     it "1" $ testProlog "f a1" "f(A1)"
  
  main :: IO ()
  main = hspec spec
  