module Problem.P027Spec (main, spec) where

import Test.Hspec
import Problem.P027

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "primesCount" $ do
    it "returns 40 when (a,b) = (1,41)" $ do
      primesCount 1 41 `shouldBe` 40
    it "returns 80 when (a,b) = (-79,1601)" $ do
      primesCount (-79) 1601 `shouldBe` 80

  describe "solve" $ do
    it "returns -235 when m = 50" $ do
      solve 50 `shouldBe` -235
    it "returns -1455 when m = 100" $ do
      solve 100 `shouldBe` -1455
