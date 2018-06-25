module Lessons.Lesson9Spec (main, spec) where
import Test.Hspec
import Lessons.Lesson9

main :: IO()
main = hspec spec

spec :: Spec
spec = do
  describe "The Fearful Symmetry" $ do
    describe "1.- List of strings" $ do
      it "handles empty string" $ do
        myWords "" `shouldBe` []
      it "handles string with spaces" $ do
        myWords "   " `shouldBe` []
      it "handles string with words" $ do
        myWords "multiple words in answer number1" `shouldBe` ["multiple", "words", "in", "answer", "number1"]
      it "handles multiple spaces between words" $ do
        myWords "  multiple   words    in  answer  number1  " `shouldBe` ["multiple", "words", "in", "answer", "number1"]
    describe "2.- Poem Lines" $ do
      it "handles multiple new lines" $ do
        myLines "\n\n\n\n" `shouldBe` ["", "", "", ""]
      it "handles words with new lines" $ do
        myLines "\nmy\npoem\nwith\nnew lines\n\n" `shouldBe` ["", "my", "poem", "with", "new lines", ""]
      it "handles empty string" $ do 
        myLines "" `shouldBe` []
    describe "3.- Square Cube" $ do
      it "retrieves the square and cube tuples" $ do
        myTuples `shouldBe` [(1,1), (1,8), (1,27), (1,64), (1,125), (4,1)
                            , (4,8), (4,27), (4,64), (4,125) ,(9,1), (9,8)
                            , (9,27), (9,64), (9,125), (16,1), (16,8), (16,27)
                            , (16,64), (16,125), (25,1), (25,8), (25,27), (25,64)
                            , (25,125)] 
      it "retrieves the square and cube tuples where x < 50 and y < 50" $ do
        myLimitedTuples `shouldBe` [(1,1), (1,8), (1,27), (4,1), (4,8), (4,27)
                                    , (9,1), (9,8), (9,27), (16,1), (16,8), (16,27)
                                    , (25,1), (25,8), (25,27)]
        myLimitedTuplesLength `shouldBe` 15
