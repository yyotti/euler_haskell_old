module Problem.P009Spec (main, spec) where

import Test.Hspec
import Problem.P009 hiding (main)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "solve" $ do
    it "returns 31875000 when n = 1000" $ do
      solve 1000 `shouldBe` 31875000
