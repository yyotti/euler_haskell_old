module Problem.P044Spec (main, spec) where

import Test.Hspec
import Problem.P044

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "solve" $ do
    it "is 5482660" $ do
      solve `shouldBe` 5482660
