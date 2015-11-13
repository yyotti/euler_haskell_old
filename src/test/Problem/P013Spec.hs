module Problem.P013Spec (main, spec) where

import Test.Hspec
import Problem.P013

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "solve" $ do
    it "returns 5537376230" $ do
      solve `shouldBe` 5537376230
