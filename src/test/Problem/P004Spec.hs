module Problem.P004Spec (main, spec) where

import Test.Hspec
import Problem.P004

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "products" $ do
    it "returns [] when n = 0" $ do
      products 0 `shouldBe` []
    it "returns [1,2,3,...,81] when n = 1" $ do
      let expected =
            [ 1,  2,  3,  4,  5,  6,  7,  8,  9,
              2,  4,  6,  8, 10, 12, 14, 16, 18,
              3,  6,  9, 12, 15, 18, 21, 24, 27,
              4,  8, 12, 16, 20, 24, 28, 32, 36,
              5, 10, 15, 20, 25, 30, 35, 40, 45,
              6, 12, 18, 24, 30, 36, 42, 48, 54,
              7, 14, 21, 28, 35, 42, 49, 56, 63,
              8, 16, 24, 32, 40, 48, 56, 64, 72,
              9, 18, 27, 36, 45, 54, 63, 72, 81]
      products 1 `shouldBe` expected
    it "returns 810 elements when n = 2" $ do
      length (products 2) `shouldBe` 810

  describe "solve" $ do
    it "returns 9 when n = 1" $ do
      solve 1 `shouldBe` 9
    it "returns 9009 when n = 2" $ do
      solve 2 `shouldBe` 9009
