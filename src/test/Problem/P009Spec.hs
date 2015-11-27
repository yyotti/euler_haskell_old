module Problem.P009Spec (main, spec) where

import Test.Hspec
import Problem.P009

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "findSpecialPythagoreanTriprets" $ do
    it "returns [] when n = 3" $ do
      findSpecialPythagoreanTriprets 3 `shouldBe` []
    it "returns Just [(3,4,5)] when n = 12" $ do
      findSpecialPythagoreanTriprets 12 `shouldBe` [(3, 4, 5)]
    it "returns Just [(6,8,10)] when n = 24" $ do
      findSpecialPythagoreanTriprets 24 `shouldBe` [(6, 8, 10)]
    it "returns Just [(5,12,13)] when n = 30" $ do
      findSpecialPythagoreanTriprets 30 `shouldBe` [(5, 12, 13)]
    it "returns Just [] when n = 32" $ do
      findSpecialPythagoreanTriprets 32 `shouldBe` []

  describe "solve" $ do
    it "returns 60000 when n = 120" $ do
      solve 120 `shouldBe` 60000
