module Problem.P001Spec (main, spec) where

import Test.Hspec
import Test.QuickCheck
import Problem.P001

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "sum'" $ do
    it "returns 1 when (a, n) = (1, 1)" $ do
      sum' (1 :: Int) 1 `shouldBe` 0
    it "returns 1 when (a, n) = (1, 2)" $ do
      sum' (1 :: Int) 2 `shouldBe` 1
    it "returns 3 when (a, n) = (1, 3)" $ do
      sum' (1 :: Int) 3 `shouldBe` 3
    it "returns 0 when (a, n) = (2, 1)" $ do
      sum' (2 :: Int) 1 `shouldBe` 0
    it "returns 0 when (a, n) = (2, 2)" $ do
      sum' (2 :: Int) 2 `shouldBe` 0
    it "returns 2 when (a, n) = (2, 3)" $ do
      sum' (2 :: Int) 3 `shouldBe` 2
    it "returns S'(a, n) of an *arbitrary* (a, n)" $ property $ do
      \a n -> a > 0 ==> let m = (n - 1) `div` a in sum' a n == (a :: Int) * (m :: Int) * (m + 1) `div` 2

  describe "solve" $ do
    it "returns 23 when max = 10" $ do
      solve 10 `shouldBe` 23
    it "returns 2418 when max = 100" $ do
      solve 100 `shouldBe` 2318
