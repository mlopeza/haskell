module Lessons.Lesson8 where
import Data.List (intercalate)

{-
8.6 Chapter Excercises
  Reviewing types
    1.- [[Bool]]
    2.- [[3==3], [6 > 5], [3 < 4]]
    3.- all of the above
    4.- func "Hello" "World"
-}


-- Reviewing currying
cattyConny :: String -> String -> String
cattyConny x y = x ++ " mrow " ++ y

flippy :: (String -> String -> String)
flippy = flip cattyConny

appedCatty :: (String -> String)
appedCatty = cattyConny "woops"

frappe :: (String -> String)
frappe = flippy "haha"

sumUpTo :: (Eq a, Num a) => a -> a
sumUpTo number = go number 0
  where go n sum
          | n == 0 = sum
          | otherwise = go (n - 1) (sum + n)

{-
  Multiply two Integrals
-}
myMultiply :: (Integral a) => a -> a -> a
myMultiply _ 0 = 0
myMultiply 0 _ = 0
myMultiply left right = retResult
 where go   l r sum
        | l == 0 = sum
        | otherwise = go (l - 1) r (sum + r)
       retResult 
        |  (left < 0) && (right < 0) = result
        |  (left < 0) || (right < 0) = (-result)
        | otherwise        = result
       result = go (abs left) (abs right) 0

data DividedResult = Result Integer | DividedByZero deriving (Show, Eq)
dividedBy :: Integer -> Integer -> DividedResult
dividedBy _ 0 = DividedByZero
dividedBy numerator denominator = Result retResult
  where go n d sum
          | n < d = sum
          | otherwise = go (n - d) d (sum + 1)
        retResult = timesResult numerator denominator result 
        result = go (toInteger $ abs numerator) (toInteger $ abs denominator) 0

timesResult :: Integer -> Integer -> Integer -> Integer
timesResult left right result
  |  (left < 0) && (right < 0) = result
  |  (left < 0) || (right < 0) = (-result)
  | otherwise        = result

mc91 :: (Ord a, Integral a) => a -> a
mc91 n
  | n > 100 = n - 10
  | otherwise = mc91 . mc91 $ n + 11


digits :: Int -> [Int]
digits 0 = [0]
digits n =  result
  where
    division = n `div` 10
    modulus = n `mod` 10
    result
      | division == 0 = [modulus]
      | division < 10 = [division, modulus]
      | otherwise = (digits division) ++ [modulus]

digitToWord :: Int -> String
digitToWord d = case d of
  0 -> "zero"
  1 -> "one"
  2 -> "two"
  3 -> "three"
  4 -> "four"
  5 -> "five"
  6 -> "six"
  7 -> "seven"
  8 -> "eight"
  9 -> "nine"
  _ -> error "Not a digit"

wordNumber :: Int -> String
wordNumber a = intercalate "-" $ map digitToWord $ digits a
