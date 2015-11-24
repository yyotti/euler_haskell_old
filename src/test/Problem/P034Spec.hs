module Problem.P034Spec (main, spec) where

import Test.Hspec
import Problem.P034

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "facts" $ do
    it "is [1,1,2,6,24,120,720,5040,40320,362880]" $ do
      facts `shouldBe` [1, 1, 2, 6, 24, 120, 720, 5040, 40320, 362880]

  describe "digitFactSum" $ do
    it "returns 6 when n = 3" $ do
      digitFactSum 3 `shouldBe` 6
    it "returns 2 when n = 10" $ do
      digitFactSum 10 `shouldBe` 2
    it "returns 2 when n = 11" $ do
      digitFactSum 11 `shouldBe` 2
    it "returns 145 when n = 145" $ do
      digitFactSum 145 `shouldBe` 145
