module Problem.P009Spec (main, spec) where

import Test.Hspec
import Problem.P009 hiding (main)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "isPythagoreanTripret" $ do
    it "returns False when (a,b,c) = (1,1,1)" $ do
      isPythagoreanTripret (1, 1, 1) `shouldBe` False
    it "returns False when (a,b,c) = (1,1,2)" $ do
      isPythagoreanTripret (1, 1, 2) `shouldBe` False
    it "returns False when (a,b,c) = (1,2,3)" $ do
      isPythagoreanTripret (1, 2, 3) `shouldBe` False
    it "returns True when (a,b,c) = (3,4,5)" $ do
      isPythagoreanTripret (3, 4, 5) `shouldBe` True
    it "returns False when (a,b,c) = (4,5,6)" $ do
      isPythagoreanTripret (4, 5, 6) `shouldBe` False
    it "returns True when (a,b,c) = (6,8,10)" $ do
      isPythagoreanTripret (6, 8, 10) `shouldBe` True
    it "returns True when (a,b,c) = (5,12,13)" $ do
      isPythagoreanTripret (5, 12, 13) `shouldBe` True

  describe "findSpecialPythagoreanTriprets" $ do
    it "returns [] when n = 3" $ do
      findSpecialPythagoreanTriprets 3 `shouldBe` []
    it "returns [(3,4,5)] when n = 12" $ do
      findSpecialPythagoreanTriprets 12 `shouldBe` [(3, 4, 5)]
    it "returns [(6,8,10)] when n = 24" $ do
      findSpecialPythagoreanTriprets 24 `shouldBe` [(6, 8, 10)]
    it "returns [(5,12,13)] when n = 30" $ do
      findSpecialPythagoreanTriprets 30 `shouldBe` [(5, 12, 13)]

  describe "solve" $ do
    it "returns 31875000 when n = 1000" $ do
      solve 1000 `shouldBe` 31875000
