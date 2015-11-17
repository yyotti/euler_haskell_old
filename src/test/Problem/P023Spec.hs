module Problem.P023Spec (main, spec) where

import Test.Hspec
import Problem.P023

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "primeFactorsExp" $ do
    it "returns [] when n = 1" $ do
      primeFactorsExp 1 `shouldBe` []
    it "returns [(2,1)] when n = 2" $ do
      primeFactorsExp 2 `shouldBe` [(2, 1)]
    it "returns [(3,1)] when n = 3" $ do
      primeFactorsExp 3 `shouldBe` [(3, 1)]
    it "returns [(2,2)] when n = 4" $ do
      primeFactorsExp 4 `shouldBe` [(2, 2)]
    it "returns [(5,1)] when n = 5" $ do
      primeFactorsExp 5 `shouldBe` [(5, 1)]
    it "returns [(2,1),(3,1)] when n = 6" $ do
      primeFactorsExp 6 `shouldBe` [(2, 1), (3, 1)]
    it "returns [(7,1)] when n = 7" $ do
      primeFactorsExp 7 `shouldBe` [(7, 1)]
    it "returns [(2,3)] when n = 8" $ do
      primeFactorsExp 8 `shouldBe` [(2, 3)]
    it "returns [(3,2)] when n = 9" $ do
      primeFactorsExp 9 `shouldBe` [(3, 2)]
    it "returns [(2,1),(5,1)] when n = 10" $ do
      primeFactorsExp 10 `shouldBe` [(2, 1), (5, 1)]
    it "returns [(11,1)] when n = 11" $ do
      primeFactorsExp 11 `shouldBe` [(11, 1)]

  describe "sumProperDivisors" $ do
    it "returns 0 when n = 1" $ do
      sumProperDivisors 1 `shouldBe` 0
    it "returns 1 when n = 2" $ do
      sumProperDivisors 2 `shouldBe` 1
    it "returns 1 when n = 3" $ do
      sumProperDivisors 3 `shouldBe` 1
    it "returns 3 when n = 4" $ do
      sumProperDivisors 4 `shouldBe` 3
    it "returns 1 when n = 5" $ do
      sumProperDivisors 5 `shouldBe` 1
    it "returns 6 when n = 6" $ do
      sumProperDivisors 6 `shouldBe` 6
    it "returns 1 when n = 7" $ do
      sumProperDivisors 7 `shouldBe` 1
    it "returns 7 when n = 8" $ do
      sumProperDivisors 8 `shouldBe` 7
    it "returns 4 when n = 9" $ do
      sumProperDivisors 9 `shouldBe` 4
    it "returns 8 when n = 10" $ do
      sumProperDivisors 10 `shouldBe` 8
    it "returns 1 when n = 11" $ do
      sumProperDivisors 11 `shouldBe` 1

  describe "isAbundantNumber" $ do
    it "returns False when n = 1" $ do
      isAbundantNumber 1 `shouldBe` False
    it "returns False when n = 10" $ do
      isAbundantNumber 10 `shouldBe` False
    it "returns True when n = 12" $ do
      isAbundantNumber 12 `shouldBe` True
    it "returns False when n = 16" $ do
      isAbundantNumber 16 `shouldBe` False
    it "returns True when n = 18" $ do
      isAbundantNumber 18 `shouldBe` True

  describe "abundantNumbers" $ do
    it "first 10 terms are [12,18,20,24,30,36,40,42,48,54]" $ do
      take 10 abundantNumbers `shouldBe` [12, 18, 20, 24, 30, 36, 40, 42, 48, 54]

  describe "solve" $ do
    it "returns 411 when n = 30" $ do
      solve 30 `shouldBe` 411
