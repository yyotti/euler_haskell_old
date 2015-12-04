module Problem.P045Spec (main, spec) where

import Test.Hspec
import Problem.P045

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "triangles" $ do
    it "first 10 terms are [1,3,6,10,15,21,28,36,45,55]" $ do
      take 10 triangles `shouldBe` [1, 3, 6, 10, 15, 21, 28, 36, 45, 55]

  describe "pentagonals" $ do
    it "first 10 terms are [1,5,12,22,35,51,70,92,117,145]" $ do
      take 10 pentagonals `shouldBe` [1, 5, 12, 22, 35, 51, 70, 92, 117, 145]

  describe "triangles" $ do
    it "first 10 terms are [1,6,15,28,45,66,91,120,153,190]" $ do
      take 10 hexagonals `shouldBe` [1, 6, 15, 28, 45, 66, 91, 120, 153, 190]

  describe "findNextSame" $ do
    it "returns 3 when (ts,ps,hs) = ([1,3,5],[1,2,3],[0,3])" $ do
      findNextSame [1, 3, 5] [1, 2, 3] [0, 3] `shouldBe` 3

  describe "solve" $ do
    it "returns 1 when n = 0" $ do
      solve 0 `shouldBe` 1
    it "returns 40755 when n = 1" $ do
      solve 1 `shouldBe` 40755
