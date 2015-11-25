module Problem.P036Spec (main, spec) where

import Test.Hspec
import Problem.P036

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "bin" $ do
    it "returns \"0\" when n = 0" $ do
      bin 0 `shouldBe` "0"
    it "returns \"1\" when n = 1" $ do
      bin 1 `shouldBe` "1"
    it "returns \"10\" when n = 2" $ do
      bin 2 `shouldBe` "10"
    it "returns \"10\" when n = 3" $ do
      bin 3 `shouldBe` "11"
    it "returns \"100\" when n = 4" $ do
      bin 4 `shouldBe` "100"
    it "returns \"101\" when n = 5" $ do
      bin 5 `shouldBe` "101"

  describe "solve" $ do
    it "returns 25 when n = 10" $ do
      solve 10 `shouldBe` 25
    it "returns 157 when n = 100" $ do
      solve 100 `shouldBe` 157
