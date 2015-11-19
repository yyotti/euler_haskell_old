module Problem.P028Spec (main, spec) where

import Test.Hspec
import Problem.P028

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "solve" $ do
    it "returns 1 when n = 1" $ do
      solve 1 `shouldBe` 1
    it "returns 25 when n = 3" $ do
      solve 3 `shouldBe` 25
    it "returns 101 when n = 5" $ do
      solve 5 `shouldBe` 101
    it "returns 0 when n is even (1)" $ do
      solve 2 `shouldBe` 0
    it "returns 0 when n is even (2)" $ do
      solve 6 `shouldBe` 0
