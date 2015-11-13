module Problem.P014Spec (main, spec) where

import Test.Hspec
import Problem.P014

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "chainLength" $ do
    it "returns 1 when n = 1" $ do
      chainLength 1 `shouldBe` 1
    it "returns 2 when n = 2" $ do
      chainLength 2 `shouldBe` 2
    it "returns 8 when n = 3" $ do
      chainLength 3 `shouldBe` 8
    it "returns 3 when n = 4" $ do
      chainLength 4 `shouldBe` 3
    it "returns 6 when n = 5" $ do
      chainLength 5 `shouldBe` 6
    it "returns 9 when n = 6" $ do
      chainLength 6 `shouldBe` 9
    it "returns 17 when n = 7" $ do
      chainLength 7 `shouldBe` 17
    it "returns 4 when n = 8" $ do
      chainLength 8 `shouldBe` 4
    it "returns 20 when n = 9" $ do
      chainLength 9 `shouldBe` 20
    it "returns 7 when n = 10" $ do
      chainLength 10 `shouldBe` 7

  describe "solve" $ do
    it "returns 27 when n = 30" $ do
      solve 30 `shouldBe` 27
    it "returns 871 when n = 1000" $ do
      solve 1000 `shouldBe` 871
