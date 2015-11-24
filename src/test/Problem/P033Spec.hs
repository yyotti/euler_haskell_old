module Problem.P033Spec (main, spec) where

import Test.Hspec
import Problem.P033

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "lct" $ do
    it "returns (2,1) when (n,d) = (2,1)" $ do
      lct (2, 1) `shouldBe` (2, 1)
    it "returns (3,2) when (n,d) = (3,2)" $ do
      lct (3, 2) `shouldBe` (3, 2)
    it "returns (1,2) when (n,d) = (1,2)" $ do
      lct (1, 2) `shouldBe` (1, 2)
    it "returns (1,2) when (n,d) = (2,4)" $ do
      lct (2, 4) `shouldBe` (1, 2)
    it "returns (2,3) when (n,d) = (6,9)" $ do
      lct (6, 9) `shouldBe` (2, 3)

  describe "incorrectLct" $ do
    it "returns (2,23) when (n,d) = (2,23)" $ do
      incorrectLct (2, 23) `shouldBe` (2, 23)
    it "returns (20,11) when (n,d) = (20,11)" $ do
      incorrectLct (20, 11) `shouldBe` (20, 11)
    it "returns (3,2) when (n,d) = (13,21)" $ do
      incorrectLct (13, 21) `shouldBe` (3, 2)
    it "returns (1,2) when (n,d) = (41,42)" $ do
      incorrectLct (41, 42) `shouldBe` (1, 2)
    it "returns (2,4) when (n,d) = (25,45)" $ do
      incorrectLct (25, 45) `shouldBe` (2, 4)
    it "returns (6,9) when (n,d) = (67,79)" $ do
      incorrectLct (67, 79) `shouldBe` (6, 9)

  describe "fractions" $ do
    it "first 10 terms are [(10,11),(10,12),(11,12),(10,13),(11,13),(12,13),(10,14),(11,14),(12,14),(13,14)]" $ do
      take 10 fractions `shouldBe` [(10, 11), (10, 12), (11, 12), (10, 13), (11, 13), (12, 13), (10, 14), (11, 14), (12, 14), (13, 14)]

  describe "isDcf" $ do
    it "returns False when f = (20,11)" $ do
      isDcf (20, 11) `shouldBe` False
    it "returns True when f = (49,98)" $ do
      isDcf (49, 98) `shouldBe` True
    it "returns False when f = (13,21)" $ do
      isDcf (13, 21) `shouldBe` False

  describe "solve" $ do
    it "is 100" $ do
      solve `shouldBe` 100
