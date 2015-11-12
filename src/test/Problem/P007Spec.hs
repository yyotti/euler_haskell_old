module Problem.P007Spec (main, spec) where

import Test.Hspec
import Problem.P007

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "solve" $ do
    it "returns 13 when n = 6" $ do
      solve 6 `shouldBe` 13
    it "returns 104743 when n = 10001" $ do
      solve 10001 `shouldBe` 104743
