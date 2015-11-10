module Problem.P004Spec (main, spec) where

import Test.Hspec
import Problem.P004 hiding (main)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "combinations" $ do
    it "returns [] when (r, ls) = (_, [])" $ do
      combinations 2 ([] :: [Char]) `shouldBe` []
    it "returns [[]] when (r, ls) = (0, _)" $ do
      combinations 0 [1 :: Int, 2, 3] `shouldBe` [[]]
    it "returns [[1],[2],[3]] when (r, ls) = (1, [1,2,3])" $ do
      combinations 1 [1 :: Int, 2, 3] `shouldBe` [[1], [2], [3]]
    it "returns [[1,2],[1,3],[2,3]] when (r, ls) = (2, [1,2,3])" $ do
      combinations 2 [1 :: Int, 2, 3] `shouldBe` [[1, 2], [1, 3], [2, 3]]
    it "returns [[1,2,3]] when (r, ls) = (3, [1,2,3])" $ do
      combinations 3 [1 :: Int, 2, 3] `shouldBe` [[1, 2, 3]]
    it "returns [] when (r, ls) = (4, [1,2,3])" $ do
      combinations 4 [1 :: Int, 2, 3] `shouldBe` []
    it "returns [\"ab\",\"ac\",\"ad\",\"bc\",\"bd\",\"cd\"] when (r, ls) = (2, \"abcd\")" $ do
      combinations 2 "abcd" `shouldBe` ["ab", "ac", "ad", "bc", "bd", "cd"]

  describe "products" $ do
    it "returns [] when n = 0" $ do
      products (0 :: Int) `shouldBe` []
    it "returns [1,2,3,...,81] when n = 1" $ do
      let expected =
            [2, 3, 4, 5, 6, 7, 8, 9,
             6, 8, 10, 12, 14, 16, 18,
             12, 15, 18, 21, 24, 27,
             20, 24, 28, 32, 36,
             30, 35, 40, 45,
             42, 48, 54,
             56, 63,
             72,
             1, 4, 9, 16, 25, 36, 49, 64, 81]
      products (1 :: Integer) `shouldBe` expected
    it "returns 4095 elements when n = 2" $ do
      length (products (2 :: Int)) `shouldBe` 4095

  describe "isPalindrome" $ do
    it "returns True when x = \"\"" $ do
      isPalindrome "" `shouldBe` True
    it "returns True when x = \"a\"" $ do
      isPalindrome "a" `shouldBe` True
    it "returns False when x = \"ab\"" $ do
      isPalindrome "ab" `shouldBe` False
    it "returns True when x = \"aa\"" $ do
      isPalindrome "aa" `shouldBe` True
    it "returns True when x = \"aca\"" $ do
      isPalindrome "aca" `shouldBe` True
    it "returns False when x = \"aac\"" $ do
      isPalindrome "aac" `shouldBe` False
    it "returns True when x = 0" $ do
      isPalindrome (0 :: Int) `shouldBe` True
    it "returns False when x = 10" $ do
      isPalindrome (10 :: Int) `shouldBe` False
    it "returns True when x = 11" $ do
      isPalindrome (11 :: Int) `shouldBe` True
    it "returns False when x = 122" $ do
      isPalindrome (122 :: Int) `shouldBe` False
    it "returns True when x = 121" $ do
      isPalindrome (121 :: Int) `shouldBe` True
    it "returns True when x = 1234321" $ do
      isPalindrome (1234321 :: Integer) `shouldBe` True

  describe "solve" $ do
    it "returns 9009 when n = 2" $ do
      solve (2 :: Int) `shouldBe` 9009
    it "returns 906609 when n = 3" $ do
      solve (3 :: Integer) `shouldBe` 906609
