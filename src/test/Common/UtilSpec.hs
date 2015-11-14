module Common.UtilSpec (main, spec) where

import Test.Hspec
import Common.Util

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "digits" $ do
    it "returns [] when n = -1" $ do
      digits (-1) `shouldBe` []
    it "returns [0] when n = 0" $ do
      digits 0 `shouldBe` [0]
    it "returns [1] when n = 1" $ do
      digits 1 `shouldBe` [1]
    it "returns [1,0] when n = 10" $ do
      digits 10 `shouldBe` [1, 0]
    it "returns [1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6,7,8,9,0] when n = 123456789012345678901234567890" $ do
      digits 123456789012345678901234567890 `shouldBe` [1, 2, 3, 4, 5, 6, 7, 8, 9, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 0]

