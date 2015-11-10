module Problem.P005Spec (main, spec) where

import Test.Hspec
import Problem.P005 hiding (main)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "solve" $ do
    it "returns 0 when (m, n) -> m < n" $ do
      solve 10 9 `shouldBe` 0
    it "returns 1 when (m, n) = (1, 1)" $ do
      solve 1 1 `shouldBe` 1
    it "returns 2 when (m, n) = (1, 2)" $ do
      solve 1 2 `shouldBe` 2
    it "returns 2 when (m, n) = (2, 2)" $ do
      solve 2 2 `shouldBe` 2
    it "returns 6 when (m, n) = (2, 3)" $ do
      solve 2 3 `shouldBe` 6
    it "returns 12 when (m, n) = (2, 4)" $ do
      solve 2 4 `shouldBe` 12
    it "returns 2520 when (m, n) = (1, 10)" $ do
      solve 1 10 `shouldBe` 2520
    it "returns 232792560 when (m, n) = (1, 20)" $ do
      solve 1 20 `shouldBe` 232792560
