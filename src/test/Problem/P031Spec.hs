module Problem.P031Spec (main, spec) where

import Test.Hspec
import Problem.P031

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "solve" $ do
    it "returns 1 when n = 1" $ do
      solve 1 `shouldBe` 1
    it "returns 2 when n = 2" $ do
      solve 2 `shouldBe` 2
    it "returns 11 when n = 10" $ do
      solve 10 `shouldBe` 11
