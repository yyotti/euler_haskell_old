module Problem.P039Spec (main, spec) where

import Test.Hspec
import Problem.P039

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "isPythagoreanTriples" $ do
    it "returns False when (a,b,c) = (1,2,3)" $ do
      isPythagoreanTriples 1 2 3 `shouldBe` False
    it "returns True when (a,b,c) = (3,4,5)" $ do
      isPythagoreanTriples 3 4 5 `shouldBe` True
    it "returns True when (a,b,c) = (5,12,13)" $ do
      isPythagoreanTriples 5 12 13 `shouldBe` True
    it "returns True when (a,b,c) = (6,8,10)" $ do
      isPythagoreanTriples 6 8 10 `shouldBe` True

  describe "pythatoreanTripletCount" $ do
    it "returns 0 when p = 1" $ do
      pythatoreanTripletCount 1 `shouldBe` 0
    it "returns 0 when p = 11" $ do
      pythatoreanTripletCount 11 `shouldBe` 0
    it "returns 1 when p = 12" $ do
      pythatoreanTripletCount 12 `shouldBe` 1
    it "returns 1 when p = 24" $ do
      pythatoreanTripletCount 24 `shouldBe` 1
    it "returns 3 when p = 120" $ do
      pythatoreanTripletCount 120 `shouldBe` 3

  describe "solve" $ do
    it "returns 12 when n = 12" $ do
      solve 12 `shouldBe` 12
    it "returns 24 when n = 24" $ do
      solve 24 `shouldBe` 24
    it "returns 120 when n = 130" $ do
      solve 130 `shouldBe` 120
