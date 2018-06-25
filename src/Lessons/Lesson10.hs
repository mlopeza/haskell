module Lessons.Lesson10 where

import Data.Time

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


