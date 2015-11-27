module Problem.P040Spec (main, spec) where

import Test.Hspec
import Problem.P040

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "solve" $ do
    it "returns 1 when ls = [1]" $ do
      solve [1] `shouldBe` 1
    it "returns 2 when ls = [1,2]" $ do
      solve [1, 2] `shouldBe` 2
    it "returns 945 when ls = [1,3,5,7,9]" $ do
      solve [1, 3, 5, 7, 9] `shouldBe` 945
    it "returns 2 when ls = [12,13,14,15,16]" $ do
      solve [12, 13, 14, 15, 16] `shouldBe` 2
