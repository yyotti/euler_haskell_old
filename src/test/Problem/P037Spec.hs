module Problem.P037Spec (main, spec) where

import Test.Hspec
import Problem.P037

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "solve" $ do
    it "returns 23 when n = 1" $ do
      solve 1 `shouldBe` 23
    it "returns 60 when n = 2" $ do
      solve 2 `shouldBe` 60
    it "returns 113 when n = 3" $ do
      solve 3 `shouldBe` 113
