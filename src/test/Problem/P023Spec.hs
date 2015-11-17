module Problem.P023Spec (main, spec) where

import Test.Hspec
import Problem.P023

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "solve" $ do
    it "is 4179871" $ do
      solve `shouldBe` 4179871
