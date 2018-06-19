module Lessons.Lesson9 where
import Data.Char
import Data.Bool

eftBool :: Bool -> Bool -> [Bool]
eftBool False False = [False]
eftBool False _ = [False, True]
eftBool True False = []
eftBool True _ = [True]

-- e.g. eftOrd LT GT
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

{- 
  1.-
  Using takeWhile and dropWhile, write a function that takes a String
  and returns a list of strings, using spaces to separate elements of the strings in words.
-}
myWords :: String -> [String]
-- terminate recursion
myWords [] = []
-- Use pattern matching and guards to split string
myWords (char:rest)
  | char == ' ' = myWords rest
  | otherwise = [char : takeLetters rest] ++ (myWords $ noLetters rest)
    where noLetters x = dropUntil ' ' x
          takeLetters x = takeUntil ' ' x

{-
  2.-
  Write a function that takes a string and returns a list of strings,
  using newline separators to breakup the string
-}
myLines :: String -> [String]
myLines [] = []
myLines (char: rest)
  -- If we receive a newline we add an empty string and continue as we could have multiple 
  -- new lines in a string
  | char == '\n' =  "" : myLines rest
  -- We get all the characters and for the next iteration we drop the first newline
  | otherwise = [char: takeUntil '\n' rest] ++ (myLines $ drop 1 $ dropUntil '\n' rest)

dropUntil :: Char -> String -> String
dropUntil char string = dropWhile (/=char) string

takeUntil :: Char -> String -> String
takeUntil char string = takeWhile (/=char) string

{-
  Square Cube
-}

mySqr :: [Integer]
mySqr = [x^2 | x <- [1..5]]
myCube :: [Integer]
myCube = [y^3 | y <- [1..5]]

myTuples :: [(Integer, Integer)]
myTuples = [(x, y) | x <- mySqr, y <- myCube]

myLimitedTuples :: [(Integer, Integer)]
myLimitedTuples = [(x, y) | (x, y) <- myTuples, x < 50 && y < 50]

myLimitedTuplesLength :: Int 
myLimitedTuplesLength = length myLimitedTuples

{-
  Bottom Madness
  1.- bottom [x^y | x <- [1..5], y <- [2, undefined]]
  2.- value  take 1 $ [x^y | x <- [1..5], y <- [2, undefined]]  
  3.- bottom sum [1, undefined, 3]
  4.- value  length [1, 2, undefined]
  5.- bottom length $ [1,2,3] ++ undefined
  6.- value  take 1 $ filter even [1, 2, 3, undefined]
  7.- bottom take 1 $ filter even [1, 3, undefined]
  8.- value  take 1 $ filter odd [1, 3, undefined]
  9.- value  take 2 $ filter odd [1, 3, undefined]
 10.- bottom take 3 $ filter odd [1, 3, undefined]
-}

{-
  Is it in normal form?
  1.- normal form
  2.- weak head 
  3.- weak head
  4.- weak head
  5.- normal form
  6.- weak head ?
  7.- weak head
-}

{-
  More Bottoms
  1.- bottom
  2.- value
  3.- bottom
  4.-
    Return a list of booleans indicating if the character in the index is a vowel
    itIsMystery :: String -> [Bool]
  5.- 
    a) All numbers from 1 to 10 squared
    b) [1, 10, 20]
    c) [15, 15 , 15]
-}

{-
  foldBool
-}
foldBool :: [Integer]
foldBool = map (\x -> bool x (-x) $ x == 3) [1..10]

multiple3 = filter (\x -> mod x 3 == 0) [1..30]
multiple3' = [x | x <- [1..30], mod x 3 == 0]

howManyMultiples = length multiple3

myFilter :: String -> [String]
myFilter w = [ x | x <- words w , x `notElem` ["the", "a", "and"]]

zip' :: [a] -> [b] -> [(a, b)]
zip' [] _ = []
zip' _ [] = []
zip' (a:ar) (b:br) = (a, b) : zip' ar br

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' fun (a:ar) (b:br) = (fun a b) : zipWith' fun ar br

zip2' = zipWith (,)


getAllUppers :: String -> String
getAllUppers str = filter isUpper str

capitalizeFirst :: String -> String
capitalizeFirst "" = ""
capitalizeFirst (f:rest) = toUpper f : rest


capitalizeFirst' "" = ""
capitalizeFirst' (f:rest) = toUpper f : capitalizeFirst' rest

getFirstCapitalized :: String -> Char
getFirstCapitalized = head . capitalizeFirst


{- End of chapter excercises -}
myOr :: [Bool] -> Bool
myOr [] = False
myOr (x: xs)
  | x == True = True
  | otherwise = myOr xs

myAny :: (a -> Bool) -> [a] -> Bool
myAny _ [] = False
myAny fun (x : xs) 
  | fun x == True = True
  | otherwise = myAny fun xs

myElem :: Eq a => a -> [a] -> Bool
myElem x all = myAny (==x) all

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x: xs) = myReverse xs ++ [x]

squish :: [[a]] -> [a]
squish [] = []
squish (x:xs) = x ++ squish xs

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap _ [] = []
squishMap fun (x: xs) = fun x ++ squishMap fun xs

squishAgain :: [[a]] -> [a]
squishAgain elements = squishMap id elements

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy _ [] = error "empty list"
myMaximumBy f e = myMaximumBy' f (tail e) (head e)

myMaximumBy' _ [] maximum = maximum
myMaximumBy' fun (x: xs) maximum
  | order == GT = myMaximumBy' fun xs x
  | otherwise = myMaximumBy' fun xs maximum
    where order = fun x maximum

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy _ [] = error "empty list"
myMinimumBy f e = myMinimumBy' f (tail e) (head e)

myMinimumBy' _ [] minimum = minimum
myMinimumBy' fun (x: xs) minimum
  | order == LT = myMinimumBy' fun xs x
  | otherwise = myMinimumBy' fun xs minimum
    where order = fun x minimum

myMaximum :: (Ord a) => [a] -> a
myMaximum = myMaximumBy compare

myMinimum :: (Ord a) => [a] -> a
myMinimum = myMinimumBy compare
