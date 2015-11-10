module Problem.P005Spec (main, spec) where

import Test.Hspec
import Problem.P005 hiding (main)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "solve" $ do
    it "returns 2520 when (m, n) = (1, 10)" $ do
      solve 1 10 `shouldBe` 2520
    it "returns 232792560 when (m, n) = (1, 20)" $ do
      solve 1 20 `shouldBe` 232792560
