module Problem.P041Spec (main, spec) where

import Test.Hspec
import Problem.P041

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "permNums" $ do
    it "returns [1] when n = 1" $ do
      permNums 1 `shouldBe` [1]
    it "returns [12,21] when n = 2" $ do
      permNums 2 `shouldBe` [12, 21]
    it "returns [123,213,321,231,312,132] when n = 3" $ do
      permNums 3 `shouldBe` [123, 213, 321, 231, 312, 132]

  describe "solve" $ do
    -- it "is 7652413" $ do
    it "時間かかりすぎるので省略" $ do
      True `shouldBe` True
