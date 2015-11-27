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

  describe "isPalindrome" $ do
    it "returns True when x = \"\"" $ do
      isPalindrome "" `shouldBe` True
    it "returns True when x = \"a\"" $ do
      isPalindrome "a" `shouldBe` True
    it "returns False when x = \"ab\"" $ do
      isPalindrome "ab" `shouldBe` False
    it "returns True when x = \"aa\"" $ do
      isPalindrome "aa" `shouldBe` True
    it "returns True when x = \"aca\"" $ do
      isPalindrome "aca" `shouldBe` True
    it "returns False when x = \"aac\"" $ do
      isPalindrome "aac" `shouldBe` False
    it "returns True when x = [0,1,2,3,2,1,0]" $ do
      isPalindrome [0, 1, 2, 3, 2, 1, 0] `shouldBe` True

  describe "isPandigital" $ do
    it "returns True when x = [123456789]" $ do
      isPandigital [123456789] `shouldBe` True
    it "returns True when x = [135792468]" $ do
      isPandigital [135792468] `shouldBe` True
    it "returns False when x = [13792468]" $ do
      isPandigital [13792468] `shouldBe` False
    it "returns True when x = [135,7,92,468]" $ do
      isPandigital [135, 7, 92, 468] `shouldBe` True

  describe "splitByComma" $ do
    it "returns [] when str = \"\"" $ do
      splitByComma "" `shouldBe` []
    it "returns [\"A\"] when str = \"A\"" $ do
      splitByComma "A" `shouldBe` ["A"]
    it "returns [\"COLIN\"] when str = \"COLIN\"" $ do
      splitByComma "COLIN" `shouldBe` ["COLIN"]
    it "returns [\"ABC\",\"DEFGHIJ\",\"KLMNO\",\"PQRST\",\"U\",\"VW\",\"XYZ\"] when str = \"ABC,DEFGHIJ,KLMNO,PQRST,U,VW,XYZ\"" $ do
      splitByComma "ABC,DEFGHIJ,KLMNO,PQRST,U,VW,XYZ" `shouldBe` ["ABC", "DEFGHIJ", "KLMNO", "PQRST", "U", "VW", "XYZ"]
