module Lessons.Lesson9 where
import Data.Char

eftBool :: Bool -> Bool -> [Bool]
eftBool False False = [False]
eftBool False _ = [False, True]
eftBool True False = []
eftBool _ _ = [False, True]

eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd a b = takeWhile (\x -> x <= b) $ enumFrom a


eftInt :: Int -> Int -> [Int]
eftInt a b = go a b
  where go current stop
           | current == max = [max]
           | current > stop = []
           | otherwise = current : go (current + 1) stop
        max = maxBound::Int

eftChar :: Char -> Char -> [Char]
eftChar a b = go (ord a) (ord b)
  where go current stop
           | current == max = [chr(max)]
           | current > stop = []
           | otherwise = chr(current) : go  (current + 1) stop
        max = ord(maxBound::Char)
