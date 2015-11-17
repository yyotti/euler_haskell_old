module Problem.P022Spec (main, spec) where

import Test.Hspec
import Problem.P022

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "calcScore" $ do
    it "returns 0 when s = \"\"" $ do
      calcScore "" `shouldBe` 0
    it "returns 53 when s = \"COLIN\"" $ do
      calcScore "COLIN" `shouldBe` 53
    it "returns 351 when s = \"ABCDEFGHIJKLMNOPQRSTUVWXYZ\"" $ do
      calcScore "ABCDEFGHIJKLMNOPQRSTUVWXYZ" `shouldBe` 351

  describe "sumNameScores" $ do
    it "returns 0 when ss = []" $ do
      sumNameScores [] `shouldBe` 0
    it "returns 0 when ss = [\"\"]" $ do
      sumNameScores [""] `shouldBe` 0
    it "returns 53 when ss = [\"COLIN\"]" $ do
      sumNameScores ["COLIN"] `shouldBe` 53
    it "returns 1559 when ss = [\"ABC\",\"DEFGHIJ\",\"KLMNO\",\"PQRST\",\"U\",\"VW\",\"XYZ\"]" $ do
      sumNameScores ["ABC", "DEFGHIJ", "KLMNO", "PQRST", "U", "VW", "XYZ"] `shouldBe` 1559

  describe "splitByComma" $ do
    it "returns [] when str = \"\"" $ do
      splitByComma "" `shouldBe` []
    it "returns [\"A\"] when str = \"A\"" $ do
      splitByComma "A" `shouldBe` ["A"]
    it "returns [\"COLIN\"] when str = \"COLIN\"" $ do
      splitByComma "COLIN" `shouldBe` ["COLIN"]
    it "returns [\"ABC\",\"DEFGHIJ\",\"KLMNO\",\"PQRST\",\"U\",\"VW\",\"XYZ\"] when str = \"ABC,DEFGHIJ,KLMNO,PQRST,U,VW,XYZ\"" $ do
      splitByComma "ABC,DEFGHIJ,KLMNO,PQRST,U,VW,XYZ" `shouldBe` ["ABC", "DEFGHIJ", "KLMNO", "PQRST", "U", "VW", "XYZ"]

  describe "readNames" $ do
    it "returns [\"COLIN\"] when str = \"src/test/resources/p022_1.txt\"" $ do
      names <- readNames "src/test/resources/p022_1.txt"
      names `shouldBe` ["COLIN"]
    it "returns [\"ALITH\",\"JACK\"] when str = \"src/test/resources/p022_2.txt\"" $ do
      names <- readNames "src/test/resources/p022_2.txt"
      names `shouldBe` ["ALITH", "JACK"]

  describe "solve" $ do
    it "returns 53 when fpath = \"src/test/resources/p022_1.txt\"" $ do
      ans <- solve "src/test/resources/p022_1.txt"
      ans `shouldBe` 53
    it "returns 100 when fpath = \"src/test/resources/p022_1.txt\"" $ do
      ans <- solve "src/test/resources/p022_2.txt"
      ans `shouldBe` 100
