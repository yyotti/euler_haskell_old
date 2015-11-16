module Common.ArithmeticSpec (main, spec) where

import Test.Hspec
import Common.Arithmetic

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "fib" $ do
    it "first 10 terms are [0,1,1,2,3,5,8,13,21,34]" $ do
      take 10 fib `shouldBe` [0, 1, 1, 2, 3, 5, 8, 13, 21, 34]

  describe "primes" $ do
    it "first 10 terms are [2,3,5,7,11,13,19,23,29,31]" $ do
      take 10 primes `shouldBe` [2, 3, 5, 7, 11, 13, 17, 19, 23, 29]

  describe "isPrime" $ do
    it "returns False when n = 1" $ do
      isPrime 1 `shouldBe` False
    it "returns True when n = 2" $ do
      isPrime 2 `shouldBe` True
    it "returns True when n = 3" $ do
      isPrime 3 `shouldBe` True
    it "returns False when n = 4" $ do
      isPrime 4 `shouldBe` False
    it "returns True when n = 5" $ do
      isPrime 5 `shouldBe` True
    it "returns False when n = 6" $ do
      isPrime 6 `shouldBe` False
    it "returns True when n = 7" $ do
      isPrime 7 `shouldBe` True
    it "returns False when n = 8" $ do
      isPrime 8 `shouldBe` False
    it "returns False when n = 9" $ do
      isPrime 9 `shouldBe` False
    it "returns False when n = 10" $ do
      isPrime 10 `shouldBe` False
    it "returns True when n = 11" $ do
      isPrime 11 `shouldBe` True

  describe "primeFactors" $ do
    it "returns [] when n = 1" $ do
      primeFactors 1 `shouldBe` []
    it "returns [2] when n = 2" $ do
      primeFactors 2 `shouldBe` [2]
    it "returns [3] when n = 3" $ do
      primeFactors 3 `shouldBe` [3]
    it "returns [2,2] when n = 4" $ do
      primeFactors 4 `shouldBe` [2, 2]
    it "returns [5] when n = 5" $ do
      primeFactors 5 `shouldBe` [5]
    it "returns [2,3] when n = 6" $ do
      primeFactors 6 `shouldBe` [2, 3]
    it "returns [7] when n = 7" $ do
      primeFactors 7 `shouldBe` [7]
    it "returns [2,2,2] when n = 8" $ do
      primeFactors 8 `shouldBe` [2, 2, 2]
    it "returns [3,3] when n = 9" $ do
      primeFactors 9 `shouldBe` [3, 3]
    it "returns [2,5] when n = 10" $ do
      primeFactors 10 `shouldBe` [2, 5]
    it "returns [11] when n = 11" $ do
      primeFactors 11 `shouldBe` [11]

  describe "fact" $ do
    it "returns 0 when n = -1" $ do
      fact (-1) `shouldBe` 0
    it "returns 1 when n = 0" $ do
      fact 0 `shouldBe` 1
    it "returns 1 when n = 1" $ do
      fact 1 `shouldBe` 1
    it "returns 2 when n = 2" $ do
      fact 2 `shouldBe` 2
    it "returns 6 when n = 3" $ do
      fact 3 `shouldBe` 6
    it "returns 24 when n = 4" $ do
      fact 4 `shouldBe` 24
