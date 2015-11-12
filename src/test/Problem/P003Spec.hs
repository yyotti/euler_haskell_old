module Problem.P003Spec (main, spec) where

import Test.Hspec
import Problem.P003

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "solve" $ do
    it "returns 29 when n = 13195" $ do
      solve 13195 `shouldBe` 29
    it "returns 6857 when n = 1234321" $ do
      solve 1234321 `shouldBe` 101
