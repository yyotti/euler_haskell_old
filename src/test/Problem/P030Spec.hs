module Problem.P030Spec (main, spec) where

import Test.Hspec
import Problem.P030

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "findMax" $ do
    it "returns 2 when n = 1" $ do
      findMax 1 `shouldBe` 2
    it "returns 3 when n = 2" $ do
      findMax 2 `shouldBe` 3
    it "returns 4 when n = 3" $ do
      findMax 3 `shouldBe` 4
    it "returns 5 when n = 4" $ do
      findMax 4 `shouldBe` 5
    it "returns 6 when n = 5" $ do
      findMax 5 `shouldBe` 6

  describe "findDigitPowers" $ do
    it "returns [] when n = 1" $ do
      findDigitPowers 1 `shouldBe` []
    it "returns [] when n = 2" $ do
      findDigitPowers 2 `shouldBe` []
    it "returns [153,370,371,407] when n = 3" $ do
      findDigitPowers 3 `shouldBe` [153, 370, 371, 407]

  describe "solve" $ do
    it "returns 1301 when n = 3" $ do
      solve 3 `shouldBe` 1301
    it "returns 19316 when n = 4" $ do
      solve 4 `shouldBe` 19316
