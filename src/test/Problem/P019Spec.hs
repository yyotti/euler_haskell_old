module Problem.P019Spec (main, spec) where

import Test.Hspec
import Problem.P019

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "solve" $ do
    it "is 171" $ do
      solve `shouldBe` 171
