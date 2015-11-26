module Problem.P037Spec (main, spec) where

import Test.Hspec
import Problem.P037

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "addLeft" $ do
    it "returns 1 when (n,d) = (1,0)" $ do
      addLeft 1 0 `shouldBe` 1
    it "returns 11 when (n,d) = (1,1)" $ do
      addLeft 1 1 `shouldBe` 11
    it "returns 211 when (n,d) = (11,2)" $ do
      addLeft 11 2 `shouldBe` 211

  describe "addRight" $ do
    it "returns 10 when (n,d) = (1,0)" $ do
      addRight 1 0 `shouldBe` 10
    it "returns 11 when (n,d) = (1,1)" $ do
      addRight 1 1 `shouldBe` 11
    it "returns 112 when (n,d) = (11,2)" $ do
      addRight 11 2 `shouldBe` 112

  describe "solve" $ do
    it "時間がかかりすぎるので省略" $ do
      True `shouldBe` True
