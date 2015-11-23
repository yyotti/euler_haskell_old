module Problem.P032Spec (main, spec) where

import Test.Hspec
import Problem.P032

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "solve" $ do
    it "45228" $ do
      solve `shouldBe` 45228
