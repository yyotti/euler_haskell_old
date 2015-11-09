module Problem.P003Spec (main, spec) where

import Test.Hspec
import Problem.P003 hiding (main)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "solve" $ do
    it "returns 29 when n = 13195" $ do
      solve 13195 `shouldBe` 29
    it "returns 6857 when n = 600851475143" $ do
      solve 600851475143 `shouldBe` 6857
