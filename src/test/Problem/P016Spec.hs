module Problem.P016Spec (main, spec) where

import Test.Hspec
import Problem.P016

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "solve" $ do
    it "returns 26 when (n, e) = (2, 15)" $ do
      solve 2 15 `shouldBe` 26
    it "returns 25 when (n, e) = (2, 16)" $ do
      solve 2 16 `shouldBe` 25
