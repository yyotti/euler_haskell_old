module Problem.P026Spec (main, spec) where

import Test.Hspec
import Problem.P026

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "recurringCycle" $ do
    it "returns [] when d = 1" $ do
      recurringCycle 1 `shouldBe` []
    it "returns [] when d = 2" $ do
      recurringCycle 2 `shouldBe` []
    it "returns [3] when d = 3" $ do
      recurringCycle 3 `shouldBe` [3]
    it "returns [] when d = 4" $ do
      recurringCycle 4 `shouldBe` []
    it "returns [] when d = 5" $ do
      recurringCycle 5 `shouldBe` []
    it "returns [6] when d = 6" $ do
      recurringCycle 6 `shouldBe` [6]
    it "returns [1,4,2,8,5,7] when d = 7" $ do
      recurringCycle 7 `shouldBe` [1,4,2,8,5,7]
    it "returns [] when d = 8" $ do
      recurringCycle 8 `shouldBe` []
    it "returns [1] when d = 9" $ do
      recurringCycle 9 `shouldBe` [1]
    it "returns [] when d = 10" $ do
      recurringCycle 10 `shouldBe` []

  describe "solve" $ do
    it "returns 7 when n = 10" $ do
      solve 10 `shouldBe` 7
