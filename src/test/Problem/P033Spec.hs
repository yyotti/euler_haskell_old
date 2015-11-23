module Problem.P033Spec (main, spec) where

import Test.Hspec
import Problem.P033

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "solve" $ do
    it "is 100" $ do
      solve `shouldBe` 100
