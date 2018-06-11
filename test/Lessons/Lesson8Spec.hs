module Lessons.Lesson8Spec (main, spec) where
import Test.Hspec
import Lessons.Lesson8

main :: IO()
main = hspec spec

spec :: Spec
spec = do
  describe "Review Currying" $ do
    describe "cattyConny" $ do
      it "appends two strings" $ do
        (cattyConny "hi" "ho") `shouldBe` "hi mrow ho"
    describe "flippy" $ do
      it "flips cattyCOnny arguments" $ do
        (flippy "hi" "ho") `shouldBe` "ho mrow hi"
    describe "appedCatty" $ do
      it "partially applies woops and only receives one argument" $ do
        (appedCatty "hi") `shouldBe` "woops mrow hi"
    describe "frappe" $ do
      it "frappe it uses flippy and partially applies haha" $ do
        (frappe "hi") `shouldBe` "hi mrow haha"


  describe "Recursion" $ do
    describe "sumUpTo" $ do
      it "sums values for 15" $ do
        (sumUpTo 15) `shouldBe` 120
      it "sums values for 1" $ do
        (sumUpTo 1) `shouldBe` 1
      it "sums values for 0" $ do
        (sumUpTo 0) `shouldBe` 0


  describe "MyMultiply" $ do
    describe "positive numbers" $ do
      it "10 times 10" $ do
        (myMultiply 10 10) `shouldBe` 100
      it "2 times 3" $ do
        (myMultiply 2 3) `shouldBe` 6
      it "one times 1" $ do
        (myMultiply 1 1) `shouldBe` 1
    describe "zero on any end" $ do
      it "0 times -10" $ do
        (myMultiply 0 (-10)) `shouldBe` 0
      it "2 times 0" $ do
        (myMultiply 2 0) `shouldBe` 0
      it "0 times 10" $ do
        (myMultiply 0 0) `shouldBe` 0
    describe "negative if any is negative" $ do
      it "-1 times 10" $ do
        (myMultiply (-1) 10) `shouldBe` (-10)
      it "2 times -1" $ do
        (myMultiply 2 (-1)) `shouldBe` (-2)
    describe "positive if both are negative" $ do
      it "-1 times -10" $ do
        (myMultiply (-1) (-10)) `shouldBe` (10)
      it "-2 times -1" $ do
        (myMultiply (-2) (-1)) `shouldBe` (2)

  describe "dividedBy" $ do
    describe "positive numbers" $ do
      it "10 divided by 10" $ do
        (dividedBy 10 10) `shouldBe` (Result 1)
      it "2 dividedBy 3" $ do
        (dividedBy 2 3) `shouldBe` (Result 0)
    describe "zero on any end" $ do
      it "0 dvidedBy -10" $ do
        (dividedBy 0 (-10)) `shouldBe` (Result 0)
      it "2 dividedBy 0" $ do
        (dividedBy 2 0) `shouldBe` DividedByZero
      it "0 dividedBy 0" $ do
        (dividedBy 0 0) `shouldBe` DividedByZero
    describe "negative if any is negative" $ do
      it "10 dividedBy -1" $ do
        (dividedBy 10 (-1)) `shouldBe` (Result (-10))
      it "-10 dividedBy 1" $ do
        (dividedBy (-10) 1) `shouldBe` (Result (-10))
    describe "positive if both are negative" $ do
      it "-10 dividedBy -10" $ do
        (dividedBy (-10) (-10)) `shouldBe` (Result 1)
      it "-10 dividedBy -5" $ do
        (dividedBy (-10) (-5)) `shouldBe` (Result 2)

  describe "McCarthy" $ do
    describe "numbers less than 100" $ do
      it "should be 91" $ do
        (mc91 0) `shouldBe` 91
        (mc91 1) `shouldBe` 91
        (mc91 2) `shouldBe` 91
        (mc91 10) `shouldBe` 91
        (mc91 20) `shouldBe` 91
        (mc91 30) `shouldBe` 91
        (mc91 99) `shouldBe` 91
        (mc91 100) `shouldBe` 91
      it "should be x - 10" $ do
        (mc91 101) `shouldBe` 91
        (mc91 102) `shouldBe` 92
        (mc91 103) `shouldBe` 93
        (mc91 104) `shouldBe` 94
        (mc91 105) `shouldBe` 95
        (mc91 106) `shouldBe` 96
  describe "WordNumber" $ do
    describe "numbers to words" $ do
      it "should convert numbers to words" $ do
        (wordNumber 0) `shouldBe` "zero"
        (wordNumber 1) `shouldBe` "one"
        (wordNumber 101) `shouldBe` "one-zero-one"
        (wordNumber 506) `shouldBe` "five-zero-six"
