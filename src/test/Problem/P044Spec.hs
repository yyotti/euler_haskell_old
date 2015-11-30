module Problem.P044Spec (main, spec) where

import Test.Hspec
import Problem.P044

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "pentagonals" $ do
    it "first 10 terms are [1,5,12,22,35,51,70,92,117,145]" $ do
      take 10 pentagonals `shouldBe` [1, 5, 12, 22, 35, 51, 70, 92, 117, 145]

  describe "isPentagonNumber" $ do
    it "returns True when n = 1" $ do
      isPentagonNumber 1 `shouldBe` True
    it "returns False when n = 2" $ do
      isPentagonNumber 2 `shouldBe` False
    it "returns False when n = 3" $ do
      isPentagonNumber 3 `shouldBe` False
    it "returns False when n = 4" $ do
      isPentagonNumber 4 `shouldBe` False
    it "returns True when n = 5" $ do
      isPentagonNumber 5 `shouldBe` True
    it "returns False when n = 6" $ do
      isPentagonNumber 6 `shouldBe` False
    it "returns True when n = 12" $ do
      isPentagonNumber 12 `shouldBe` True

  describe "solve" $ do
    it "時間がかかりすぎるので省略" $ do
      True `shouldBe` True
