module Problem.P021Spec (main, spec) where

import Test.Hspec
import Problem.P021

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "primeFactorsCount" $ do
    it "returns [] when n = 1" $ do
      primeFactorsCount 1 `shouldBe` []
    it "returns [(2,1)] when n = 2" $ do
      primeFactorsCount 2 `shouldBe` [(2, 1)]
    it "returns [(3,1)] when n = 3" $ do
      primeFactorsCount 3 `shouldBe` [(3, 1)]
    it "returns [(2,2)] when n = 4" $ do
      primeFactorsCount 4 `shouldBe` [(2, 2)]
    it "returns [(5,1)] when n = 5" $ do
      primeFactorsCount 5 `shouldBe` [(5, 1)]
    it "returns [(2,1),(3,1)] when n = 6" $ do
      primeFactorsCount 6 `shouldBe` [(2, 1), (3, 1)]
    it "returns [(7,1)] when n = 7" $ do
      primeFactorsCount 7 `shouldBe` [(7, 1)]
    it "returns [(2,3)] when n = 8" $ do
      primeFactorsCount 8 `shouldBe` [(2, 3)]
    it "returns [(3,2)] when n = 9" $ do
      primeFactorsCount 9 `shouldBe` [(3, 2)]
    it "returns [(2,1),(5,1)] when n = 10" $ do
      primeFactorsCount 10 `shouldBe` [(2, 1), (5, 1)]
    it "returns [(11,1)] when n = 11" $ do
      primeFactorsCount 11 `shouldBe` [(11, 1)]

  describe "factorsSum" $ do
    it "returns 1 when n = 1" $ do
      factorsSum 1 `shouldBe` 1
    it "returns 3 when n = 2" $ do
      factorsSum 2 `shouldBe` 3
    it "returns 4 when n = 3" $ do
      factorsSum 3 `shouldBe` 4
    it "returns 7 when n = 4" $ do
      factorsSum 4 `shouldBe` 7
    it "returns 6 when n = 5" $ do
      factorsSum 5 `shouldBe` 6
    it "returns 12 when n = 6" $ do
      factorsSum 6 `shouldBe` 12
    it "returns 8 when n = 7" $ do
      factorsSum 7 `shouldBe` 8
    it "returns 15 when n = 8" $ do
      factorsSum 8 `shouldBe` 15
    it "returns 13 when n = 9" $ do
      factorsSum 9 `shouldBe` 13
    it "returns 18 when n = 10" $ do
      factorsSum 10 `shouldBe` 18
    it "returns 12 when n = 11" $ do
      factorsSum 11 `shouldBe` 12

  describe "amicableNum" $ do
    it "returns [1,0] when (m,n) = (1,1)" $ do
      amicableNum 1 1 `shouldBe` [1,0]
    it "returns [1,0] when (m,n) = (10,1)" $ do
      amicableNum 10 1 `shouldBe` [1,0]
    it "returns [] when (m,n) = (10,2)" $ do
      amicableNum 10 2 `shouldBe` []
    it "returns [] when (m,n) = (10,6)" $ do
      amicableNum 10 6 `shouldBe` []
    it "returns [220,284] when (m,n) = (500,220)" $ do
      amicableNum 500 220 `shouldBe` [220, 284]
    it "returns [] when (m,n) = (250,220)" $ do
      amicableNum 250 220 `shouldBe` []

  describe "solve" $ do
    it "returns 504 when n = 1000" $ do
      solve 1000 `shouldBe` 504
    it "returns 8442 when n = 3000" $ do
      solve 3000 `shouldBe` 8442
