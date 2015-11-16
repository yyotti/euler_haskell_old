module Problem.P020Spec (main, spec) where

import Test.Hspec
import Problem.P020

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "solve" $ do
    it "returns 27 when n = 10" $ do
      solve 10 `shouldBe` 27
    it "returns 54 when n = 20" $ do
      solve 20 `shouldBe` 54
