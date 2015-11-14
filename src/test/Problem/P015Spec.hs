module Problem.P015Spec (main, spec) where

import Test.Hspec
import Problem.P015

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "solve" $ do
    it "returns 6 when n = 2" $ do
      solve 2 `shouldBe` 6
    it "returns 515 when n = 10" $ do
      solve 10 `shouldBe` 515
