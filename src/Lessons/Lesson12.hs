module Lessons.Lesson12 where
import Data.Char
import Data.List
{-
 1.- *->*
 2.- * -> *
     *
-}

notThe :: String -> Maybe String
notThe str
  | (map toLower str) == "the" = Nothing
  | otherwise = Just str

replaceThe :: String -> String
replaceThe str = intercalate " " $ go wordList
  where 
   go :: [Maybe [Char]] -> [[Char]]
   go [] = []
   go (x: xs) = case x of
                Nothing -> "a" : go xs
                Just val -> val : go xs
   wordList = map notThe (words str)

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel str = go wordList
  where
   go :: [Maybe [Char]] -> Integer
   go [] = 0 
   go (Just _: rest) = go rest 
   go (Nothing: Just val: rest) =  (containsVowel val) + go rest
   go (Nothing: rest) = go rest
   containsVowel :: [Char] -> Integer
   containsVowel [] = 0
   containsVowel word = if elem (toLower (head word)) "aeiou" then 1 else 0
   wordList = map notThe (words str)

countVowels :: String -> Integer
countVowels str = sum [ 1 | x <- str, elem (toLower x) "aeiou" ]

newtype Word' = Word' String deriving (Eq, Show)
vowels = "aeiou"
notVowels = filter (`notElem` vowels) ['a' .. 'z']

countCons :: String -> Integer
countCons str = sum [ 1 | x <- str, elem (toLower x) notVowels ]


mkWord :: String -> Maybe Word'
mkWord str = if vowels > consonants then Nothing else Just $ Word' str
 where
  vowels = countVowels str
  consonants = countCons str

data Nat = Zero | Succ Nat  deriving (Eq, Show)
natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ val) = 1 + natToInteger val

integerToNat :: Integer -> Maybe Nat
integerToNat x = result
 where
  go 0 = Zero
  go x = Succ (go $ x - 1)
  result = if x < 0 then Nothing else Just $ go x

isJust :: Maybe a -> Bool
isJust Nothing = False
isJust _ = True

isNothing :: Maybe a -> Bool
isNothing = not . isJust

mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee val _ Nothing = val
mayybee _ fun (Just x) = fun x

fromMaybe :: a -> Maybe a -> a
fromMaybe val Nothing = val
fromMaybe _ (Just val) = val

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (x: xs) = Just x

maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just val) = [val]

catMaybes :: [Maybe a] -> [a]
catMaybes list = [x | (Just x) <- list]


flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe values = if onlyJust then (Just justValues) else Nothing
 where 
  justValues = catMaybes values
  onlyJust = (length justValues) == length values

lefts' :: [Either a b] -> [a]
lefts' values = catMaybes $ map (getLeft) values 
 where
  getLeft (Left x) = Just x
  getLeft _ = Nothing

rights' :: [Either a b] -> [b]
rights' values = catMaybes $ map (getRight) values 
 where
  getRight (Right x) = Just x
  getRight _ = Nothing


partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' values = (lefts' values, rights' values)

eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' _ (Left _) = Nothing
eitherMaybe' fun (Right val) = Just $ fun val

either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' fun1 _ (Left val) = fun1 val
either' _ fun2 (Right val) = fun2 val

eitherMaybe2' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe2' fun = either' (\_ -> Nothing) (\x -> Just $ fun x)


myIterate :: (a -> a) -> a -> [a]
myIterate fun val = val : myIterate fun (fun val)

myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr fun seed = go (fun seed)
 where
  go Nothing = []
  go (Just (x, y)) = x : go (fun y)

betterIterate :: (a -> a) -> a -> [a]
betterIterate fun val = myUnfoldr (\x -> Just (x , (fun x))) val


data BinaryTree a = Leaf | Node (BinaryTree a) a (BinaryTree a) deriving (Eq, Ord, Show)

treeBuild :: Integer -> BinaryTree Integer
treeBuild n = go 0
 where
  go current
    | current == n = Leaf
    | otherwise = Node (go $ current + 1) current (go $ current + 1)
