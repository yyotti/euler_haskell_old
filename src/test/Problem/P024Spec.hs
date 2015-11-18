module Problem.P024Spec (main, spec) where

import Test.Hspec
import Problem.P024

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "solve" $ do
    it "returns 120 when (ns, i) = ([0,1,2], 4)" $ do
      solve [0, 1, 2] 4 `shouldBe` 120
