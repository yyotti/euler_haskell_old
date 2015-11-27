module Problem.P041Spec (main, spec) where

import Test.Hspec
import Problem.P041

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "solve" $ do
    it "is 7652413" $ do
      solve `shouldBe` 7652413
