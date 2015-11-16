module Problem.P019Spec (main, spec) where

import Test.Hspec
import Problem.P019

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "isLeapYear" $ do
    it "returns False when y = 1900" $ do
      isLeapYear 1900 `shouldBe` False
    it "returns False when y = 1901" $ do
      isLeapYear 1901 `shouldBe` False
    it "returns False when y = 1902" $ do
      isLeapYear 1902 `shouldBe` False
    it "returns False when y = 1903" $ do
      isLeapYear 1903 `shouldBe` False
    it "returns True when y = 1904" $ do
      isLeapYear 1904 `shouldBe` True
    it "returns True when y = 2000" $ do
      isLeapYear 2000 `shouldBe` True

  describe "dateCount" $ do
    it "returns [31,28,31,30,31,30,31,31,30,31,30,31] when (y,_) = (1900,_)" $ do
      map (dateCount 1900) [1..12] `shouldBe` [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    it "returns [31,28,31,30,31,30,31,31,30,31,30,31] when (y,_) = (1901,_)" $ do
      map (dateCount 1901) [1..12] `shouldBe` [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    it "returns [31,28,31,30,31,30,31,31,30,31,30,31] when (y,_) = (1902,_)" $ do
      map (dateCount 1902) [1..12] `shouldBe` [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    it "returns [31,28,31,30,31,30,31,31,30,31,30,31] when (y,_) = (1903,_)" $ do
      map (dateCount 1903) [1..12] `shouldBe` [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    it "returns [31,29,31,30,31,30,31,31,30,31,30,31] when (y,_) = (1904,_)" $ do
      map (dateCount 1904) [1..12] `shouldBe` [31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    it "returns [31,29,31,30,31,30,31,31,30,31,30,31] when (y,_) = (2000,_)" $ do
      map (dateCount 2000) [1..12] `shouldBe` [31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]

  describe "solve" $ do
    it "is 171" $ do
      solve `shouldBe` 171
