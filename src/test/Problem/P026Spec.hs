module Problem.P026Spec (main, spec) where

import Test.Hspec
import Problem.P026

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "solve" $ do
    it "returns 7 when n = 10" $ do
      solve 10 `shouldBe` 7
