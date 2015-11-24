module Problem.P034Spec (main, spec) where

import Test.Hspec
import Problem.P034

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "solve" $ do
    it "is 40730" $ do
      solve `shouldBe` 40730
