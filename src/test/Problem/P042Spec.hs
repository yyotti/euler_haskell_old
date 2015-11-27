module Problem.P042Spec (main, spec) where

import Test.Hspec
import Problem.P042

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "isSquare" $ do
    it "returns True when n = 1" $ do
      isSquare 1 `shouldBe` True
    it "returns False when n = 2" $ do
      isSquare 2 `shouldBe` False
    it "returns False when n = 3" $ do
      isSquare 3 `shouldBe` False
    it "returns True when n = 4" $ do
      isSquare 4 `shouldBe` True
    it "returns False when n = 5" $ do
      isSquare 5 `shouldBe` False
    it "returns True when n = 9" $ do
      isSquare 9 `shouldBe` True

  describe "isTriangleNumber" $ do
    it "returns True when n = 1" $ do
      isTriangleNumber 1 `shouldBe` True
    it "returns False when n = 2" $ do
      isTriangleNumber 2 `shouldBe` False
    it "returns True when n = 3" $ do
      isTriangleNumber 3 `shouldBe` True
    it "returns False when n = 4" $ do
      isTriangleNumber 4 `shouldBe` False
    it "returns False when n = 5" $ do
      isTriangleNumber 5 `shouldBe` False
    it "returns True when n = 6" $ do
      isTriangleNumber 6 `shouldBe` True
    it "returns True when n = 55" $ do
      isTriangleNumber 55 `shouldBe` True

  describe "wordValue" $ do
    it "returns 0 when s = \"\"" $ do
      wordValue "" `shouldBe` 0
    it "returns 53 when s = \"COLIN\"" $ do
      wordValue "COLIN" `shouldBe` 53
    it "returns 351 when s = \"ABCDEFGHIJKLMNOPQRSTUVWXYZ\"" $ do
      wordValue "ABCDEFGHIJKLMNOPQRSTUVWXYZ" `shouldBe` 351

  describe "countTriangleWords" $ do
    it "returns 0 when ss = []" $ do
      countTriangleWords [] `shouldBe` 0
    it "returns 1 when ss = [\"A\"]" $ do
      countTriangleWords ["A"] `shouldBe` 1
    it "returns 1 when ss = [\"AAA\"]" $ do
      countTriangleWords ["AAA"] `shouldBe` 1
    it "returns 0 when ss = [\"ABA\"]" $ do
      countTriangleWords ["ABA"] `shouldBe` 0
    it "returns 3 when ss = [\"A\",\"AAA\",\"ABCBA\",\"SKY\"]" $ do
      countTriangleWords ["A", "AAA", "ABCBA", "SKY"] `shouldBe` 3

  describe "readWords" $ do
    it "returns [\"SKY\"] when fpath = \"src/test/resources/p042_1.txt\"" $ do
      names <- readWords "src/test/resources/p042_1.txt"
      names `shouldBe` ["SKY"]
    it "returns [\"A\",\"ABILITY\",\"ABLE\",\"ABOUT\",\"ABOVE\"] when fpath = \"src/test/resources/p042_2.txt\"" $ do
      names <- readWords "src/test/resources/p042_2.txt"
      names `shouldBe` ["A", "ABILITY", "ABLE", "ABOUT", "ABOVE"]


  describe "solve" $ do
    it "returns 1 when fpath = \"src/test/resources/p042_1.txt\"" $ do
      ans <- solve "src/test/resources/p042_1.txt"
      ans `shouldBe` 1
    it "returns 2 when fpath = \"src/test/resources/p042_2.txt\"" $ do
      ans <- solve "src/test/resources/p042_1.txt"
      ans `shouldBe` 1
