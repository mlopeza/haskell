module Lessons.Lesson10 where

import Data.Time

if' :: Bool -> a -> a -> a
if' True  x _ = x
if' False _ y = y

{-Excercises: Understanding Folds

1. b, c
2. (((1 * 1) * 2) * 3)
3. c
4. a
5. 
  a) foldr (:) [] ["woot", "WOOT", "woot"]
  b) foldr max (minBound::Char) "fear is the little death" 
  c) foldr (&&) True [False, True]
  d) no, it short-circuits on the first True
  e) no errors..
  f) foldr const 'a' ['x']
  g) foldr const 'a' "tacos"
  h) foldl (flip const) 'a' "burritos"
  i) foldl (flip const) 10 [1..5]
-}

{- Excercises: Database processing -}

data DatabaseItem = DbString String
                  | DbNumber Integer
                  | DbDate UTCTime
                  deriving (Eq, Ord, Show)
theDatabase :: [DatabaseItem]
theDatabase = 
  [ DbDate (UTCTime (fromGregorian 1911 5 1) (secondsToDiffTime 34123))
  , DbNumber 9001
  , DbString "Hello, World!"
  , DbDate (UTCTime (fromGregorian 1921 5 1) (secondsToDiffTime 34123))
  ]


filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate e = [t | (DbDate t) <- e]


filterDbDate' :: [DatabaseItem] -> [UTCTime]
filterDbDate' [] = []
filterDbDate' (DbDate x: tail) = x : filterDbDate' tail
filterDbDate' (_: tail) = filterDbDate' tail


filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber e = [t | (DbNumber t) <- e]

mostRecent :: [DatabaseItem] -> UTCTime
mostRecent = maximum . filterDbDate

sumDb :: [DatabaseItem] -> Integer
sumDb = sum . filterDbNumber

avgDb :: [DatabaseItem] -> Double
avgDb db = a / b
  where result = avgDb' db
        a = fromIntegral $ fst result
        b = fromIntegral $ snd result

avgDb' :: [DatabaseItem] -> (Integer, Integer)
avgDb' db = foldr (\x y -> ((fst y + x), (snd y + 1))) (0, 0) $ filterDbNumber theDatabase

{- Chapter 10 Excercises -}

-- 1.
stops = "pbtdkg"
vowels = "aeiou"
combinations s v = [(x, y, z) | x <- s, y <- v, z <- s]
combinationsP s v= [ (x, y, z) | (x, y, z) <- combinations s v, x == 'p']

-- 2 average word size
seekritFunc x = div (sum . map length $ words x ) (length $ words x)

seekritFuncPrecise x = fromIntegral (sum . map length $ words x ) / fromIntegral (length $ words x)

myAnd :: [Bool] -> Bool
myAnd = foldr (&&) True

myOr :: [Bool] -> Bool
myOr = foldr (||) False

myAny :: (a -> Bool) -> [a] -> Bool
myAny function = foldr ((||) . function) False

myElem :: Eq a => a -> [a] -> Bool
myElem value = myAny (\x -> x == value)

myReverse :: [a] -> [a]
myReverse = foldr (\x y -> y ++ [x]) []

myMap :: (a -> b) -> [a] -> [b]
myMap func = foldr (\x y -> func x : y) []

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter func = foldr (\x y -> if' (func x) (x : y) y) []

squish :: [[a]] -> [a]
squish = foldr (\x y -> x ++ y) []

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap func = foldr (\x y -> func x ++ y) []

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy _ [] = error "empty list"
myMaximumBy func list = foldl (\a b -> if' (func a b == GT) a b) (head list) list

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy _ [] = error "empty list"
myMinimumBy func list = foldl (\a b -> if' (func a b == LT) a b) (head list) list
