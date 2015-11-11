module Problem.P004Spec (main, spec) where

import Test.Hspec
import Problem.P004 hiding (main)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "products" $ do
    it "returns [] when n = 0" $ do
      products (0 :: Int) `shouldBe` []
    it "returns [1,2,3,...,81] when n = 1" $ do
      let expected =
            [1, 2, 3, 4, 5, 6, 7, 8, 9,
             2, 4, 6, 8, 10, 12, 14, 16, 18,
             3, 6, 9, 12, 15, 18, 21, 24, 27,
             4, 8, 12, 16, 20, 24, 28, 32, 36,
             5, 10, 15, 20, 25, 30, 35, 40, 45,
             6, 12, 18, 24, 30, 36, 42, 48, 54,
             7, 14, 21, 28, 35, 42, 49, 56, 63,
             8, 16, 24, 32, 40, 48, 56, 64, 72,
             9, 18, 27, 36, 45, 54, 63, 72, 81]
      products (1 :: Integer) `shouldBe` expected
    it "returns 810 elements when n = 2" $ do
      length (products (2 :: Int)) `shouldBe` 810

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
