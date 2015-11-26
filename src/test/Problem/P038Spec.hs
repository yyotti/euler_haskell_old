module Problem.P038Spec (main, spec) where

import Test.Hspec
import Problem.P038

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "solve" $ do
    it "is 932718654" $ do
      solve `shouldBe` 932718654
