module Lessons.Cipher where
import Data.Char

alphabet = ['A'..'Z']

cipher num x
  | cipherCharNumber >= 91 = chr $ (cipherCharNumber `mod` 90)  + 64
  | cipherCharNumber >= 65 = chr cipherCharNumber
  | otherwise = error "Invalid number"
    where cipherCharNumber = (ord x) + num

uncipher num x
  | cipherCharNumber < 65 = chr (90 - (abs (64 - cipherCharNumber)))
  | cipherCharNumber <= 90 = chr cipherCharNumber
  | otherwise = error "Invalid number"
    where cipherCharNumber = (ord x) - num

shift fun num [] = []
shift fun num (x: xs)
  | x == ' ' = x : shift fun num xs
  | otherwise = fun num x : shift fun num xs

caesarCipher num text = shift cipher (num `mod` 26) $ cleanData text
unCaesar num text = shift uncipher (num `mod` 26) $ cleanData text 

cleanData text = filter (\x -> x == ' ' || x `elem` alphabet) $ map toUpper text
