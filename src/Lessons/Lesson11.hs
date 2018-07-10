{-# LANGUAGE  FlexibleInstances #-}
module Lessons.Lesson11 where
import Data.Char
import Data.List
data Size = AirBus380 | Boeing777 deriving (Eq, Show)
data Price  = Price Integer deriving (Eq, Show)
data Manufacturer = Mini | Mazda | Tata deriving (Eq, Show)
data Airline = PapuAir | CatapultsR'Us | TakeYourChancesUnited deriving (Eq, Show)
data Vehicle = Car Manufacturer Price | Plane Airline Size deriving (Eq, Show)


myCar = Car Mini (Price 14000)
urCar = Car Mazda (Price 20000)
clownCar = Car Tata (Price 7000)
doge = Plane PapuAir AirBus380
{-
1- What is the type of myCar?
  Vehicle
-}

-- 2 GIven the following define the functions
isCar :: Vehicle -> Bool
isCar (Car _ _) = True
isCar _ = False

isPlane :: Vehicle -> Bool
isPlane (Plane _ _) = True
isPlane _ = False

areCars :: [Vehicle] -> Bool
areCars = foldr (\x y -> isCar x && y) True

-- 3 Tell us teh manufacturer of a piece of data
getManu :: Vehicle -> Manufacturer
getManu (Car manu _) = manu
getManu _ = error "No manufacturer"

-- It will fail with an error
data Example = MakeExample deriving Show
data Example2 = MakeExample2 Int deriving Show

newtype Goats  = Goats Int deriving (Eq, Show)
newtype Cows = Cows Int deriving (Eq, Show)

class TooMany a where
  tooMany :: a -> Bool

instance TooMany Int where
  tooMany n = n > 42

instance TooMany Goats where
  tooMany (Goats n) = n > 43

instance TooMany (Int, String) where
  tooMany (n, _) = n > 42

--instance TooMany (Int, Int) where
--  tooMany (a, b) = a + b > 42

-- How? Not sure how this works
instance (Num a, TooMany a) => TooMany (a, a) where
  tooMany (a, b) = tooMany $ a + b


{- Pity the bool -}
{-
1- 4
2- 258 (Int8 + Bool)

-}

data BinaryTree a = Leaf | Node (BinaryTree a) a (BinaryTree a) deriving (Eq, Ord, Show)

insert' :: Ord a 
              => a
              -> BinaryTree a
              -> BinaryTree a
insert' b Leaf = Node Leaf b Leaf
insert' b (Node left a right)
  | b == a = Node left a right
  | b < a = Node (insert' b left) a right
  | b > a = Node left a (insert' b right)

mapTree :: (a->b) -> BinaryTree a -> BinaryTree b
mapTree _ Leaf = Leaf
mapTree f (Node left a right) =
  Node (mapTree f left) (f a) (mapTree f right)

testTree' :: BinaryTree Integer
testTree' = Node 
            (Node Leaf 3 Leaf)
              1
            (Node Leaf 4 Leaf)

mapExpected = Node
              (Node Leaf 4 Leaf)
              2
              (Node Leaf 5 Leaf)

preorder :: BinaryTree a -> [a]
preorder Leaf = []
preorder (Node a b c) = b : preorder a ++ preorder c

inorder :: BinaryTree a -> [a]
inorder Leaf = []
inorder (Node a b c) = inorder a ++ [b] ++ inorder c

postOrder :: BinaryTree a -> [a]
postOrder Leaf = []
postOrder (Node a b c) = postOrder a ++ postOrder c ++ [b]

testTree :: BinaryTree Integer
testTree =  Node
            (Node Leaf 1 Leaf)
            2
            (Node Leaf 3 Leaf)

foldTree :: (a -> b -> b) -> b -> BinaryTree a -> b
foldTree fun var ele = foldr fun var $ inorder ele

foldRes = foldTree (+) 0 testTree


{-11.8

1.- a
2.- c
3.- b
4.- a, c ?
-}


-- Only uppercase words
vigenere :: ((Char, Char) -> Char) -> String -> String -> String
vigenere fun key plaintext = map fun result
  where result = wrapWord 
                  (map toUpper (cycle (filter (\x -> x /= ' ') key))) -- key all uppercase
                  (map toUpper plaintext) -- plain text all uppercase

vigenereCipher :: String -> String -> String
vigenereCipher = vigenere caesars
 
vigenereUncipher :: String -> String -> String
vigenereUncipher = vigenere unCaesars

-- Cipher a character based on the key
caesars :: (Char, Char) -> Char
caesars (_, ' ') = ' '
caesars (key, plain) = chr((mod ((ord key - 65) + (ord plain - 65)) 26) + 65)

unCaesars :: (Char, Char) -> Char
unCaesars (_, ' ') = ' '
unCaesars (key, plain) = chr ((mod (26 + (ord plain - 65) - (ord key - 65)) 26) + 65)

-- Create list of keys and cipher characters skipping spaces
wrapWord :: String -> String -> [(Char, Char)]
wrapWord [] plainText = error "Empty cypher word"
wrapWord _ [] = []
wrapWord cip@(x: xs) pl@(y: ys)
    | y == ' ' = (' ', ' ') : wrapWord cip ys
    | otherwise = (x, y) : wrapWord xs ys
    
-- Remove white space from keywords
removeWhiteSpace :: String -> String 
removeWhiteSpace str = [x | x <- str , x /= ' ']

isSubSeqOf :: (Eq a) => [a] -> [a] -> Bool
isSubSeqOf [] [] = True
isSubSeqOf [] _ = True
isSubSeqOf _ [] = False
isSubSeqOf a@(x:xs) b@(y:ys)
      | x == y = isSubSeqOf xs ys
      | otherwise = isSubSeqOf a ys

capitalizeWord :: String -> String
capitalizeWord (x: xs) = toUpper x : xs
capitalizeWord _ = []

capitalizeWords :: String -> [(String, String)]
capitalizeWords elements = [(x, capitalizeWord x) | x <- words elements]

-- Don't use capitalize words, how do you write it capitalizing words?
capitalizeParagraph :: String -> String
capitalizeParagraph p = go p True
  where
    go [] _ = [] 
    go ('.': xs) _ = '.' : go xs True
    go (' ': xs) afterDot = ' ': go xs afterDot
    go (x :  xs) True = (toUpper x) : go xs False
    go (x :  xs) afterDot = x : go xs afterDot

{- Phone excercise -}

type Digit = Char
type Presses = Int
data Button = Button Digit String deriving (Eq, Show)
data Phone = Phone [Button] deriving (Eq, Show)

buttons (Phone b) = b

phone = Phone [
  Button '0' " ", 
  Button '1' "",   
  Button '2' "abc",   
  Button '3' "def",   
  Button '4' "ghi",   
  Button '5' "jkl", 
  Button '6' "mno",
  Button '7' "pqrs", 
  Button '8' "tuv", 
  Button '9' "wxyz", 
  Button '*' "", 
  Button '#' ".," ]

convo = ["Wanna play 20 questions"]

-- Ugly function
reverseTaps :: Phone -> Char -> [(Digit, Presses)]
reverseTaps phone character = result lookup
  where
    result [] = error "Invalid character"
    result x = if isUpper character then ('*', 1) : x else x
    lookup = map (\(Button digit chars) -> (digit, getIndex $ elemIndex char chars)) filtering
    filtering = filter (\(Button digit chars) -> elem char chars) $ buttons phone 
    getIndex Nothing = error "Invalid Character"
    getIndex (Just val) = val + 1 -- Add 1 to the index
    char = toLower character


cellPhonesDead :: Phone -> String -> [(Digit, Presses)]
cellPhonesDead phone = concat . map (reverseTaps phone)

fingerTaps :: [(Digit, Presses)] -> Presses
fingerTaps = foldr (\(a, b) y -> b + y) 0

mostPopularLetter :: String -> Char
mostPopularLetter string = fst result
  where 
    result = foldr (\(x, y) (a, b)-> if y > b then (x, y) else (a, b)) 
              (head reduceElements) reduceElements
    reduceElements = foldr addToResult [] mapElements
    mapElements = map (\x -> (x, 1)) string
    addToResult x [] = [x]
    addToResult c@(x, y) (e@(a, b): rest)
              | x == a = (x, y + b): rest
              | otherwise = e : addToResult c rest
    
coolestLtr :: [String] -> Char
coolestLtr = mostPopularLetter . concat

coolestWord :: [String] -> String
coolestWord elements = fst result
  where 
    result = foldr (\(x, y) (a, b)-> if y > b then (x, y) else (a, b)) 
              (head reduceElements) reduceElements
    reduceElements = foldr addToResult [] mapElements
    mapElements = map (\x -> (x, 1)) $ words $ intercalate " " elements
    addToResult x [] = [x]
    addToResult c@(x, y) (e@(a, b): rest)
              | x == a = (x, y + b): rest
              | otherwise = e : addToResult c rest


data Expr = Lit Integer
  | Add Expr Expr


eval :: Expr -> Integer
eval (Lit a) = a
eval (Add a b) = (eval a) + (eval b)


printExpr :: Expr -> String
printExpr (Lit a) = show a
printExpr (Add a b) = (printExpr a) ++ " + " ++ (printExpr b)


