module Problem.P037Spec (main, spec) where

import Test.Hspec
import Problem.P037

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "trimr" $ do
    it "returns 0 when n = 1" $ do
      trimr 1 `shouldBe` 0
    it "returns 0 when n = 9" $ do
      trimr 9 `shouldBe` 0
    it "returns 3 when n = 39" $ do
      trimr 39 `shouldBe` 3
    it "returns 53 when n = 539" $ do
      trimr 539 `shouldBe` 53

  describe "triml" $ do
    it "returns 0 when n = 1" $ do
      triml 1 `shouldBe` 0
    it "returns 0 when n = 9" $ do
      triml 9 `shouldBe` 0
    it "returns 9 when n = 39" $ do
      triml 39 `shouldBe` 9
    it "returns 39 when n = 539" $ do
      triml 539 `shouldBe` 39

  describe "truncates" $ do
    it "returns [1] when n = 1" $ do
      truncates 1 `shouldBe` [1]
    it "returns [9] when n = 9" $ do
      truncates 9 `shouldBe` [9]
    it "returns [39,3,9] when n = 39" $ do
      truncates 39 `shouldBe` [39,3,9]
    it "returns [539,53,5,39,9] when n = 539" $ do
      truncates 539 `shouldBe` [539, 53, 5, 39, 9]
    it "returns [1111,111,11,1] when n = 1111" $ do
      truncates 1111 `shouldBe` [1111, 111, 11, 1]

  describe "isTruncatablePrime" $ do
    it "returns False when n = 2" $ do
      isTruncatablePrime 2 `shouldBe` False
    it "returns False when n = 3" $ do
      isTruncatablePrime 3 `shouldBe` False
    it "returns False when n = 5" $ do
      isTruncatablePrime 5 `shouldBe` False
    it "returns False when n = 7" $ do
      isTruncatablePrime 7 `shouldBe` False
    it "returns False when n = 17" $ do
      isTruncatablePrime 17 `shouldBe` False
    it "returns True when n = 37" $ do
      isTruncatablePrime 37 `shouldBe` True
    it "returns True when n = 3797" $ do
      isTruncatablePrime 3797 `shouldBe` True

  describe "solve" $ do
    it "returns 23 when n = 1" $ do
      solve 1 `shouldBe` 23
    it "returns 60 when n = 2" $ do
      solve 2 `shouldBe` 60
    it "returns 113 when n = 3" $ do
      solve 3 `shouldBe` 113
