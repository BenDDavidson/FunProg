{- Week6.hs
 This module illustrates the use of functions as values
-}

import Data.Char

twice :: (Int -> Int) -> Int -> Int
twice f x = f (f x)

multiply :: Int -> Int -> Int
multiply x y = x * y

double :: Int -> Int
double = multiply 2

doubleAll :: [Int] -> [Int]
doubleAll = map (*2)

areDigits :: String -> [Bool]
areDigits = map isDigit

keepPositive :: [Int] -> [Int]
keepPositive = filter (>0)

keepDigits :: String -> String
keepDigits = filter isDigit

addUp :: [Int] -> Int
addUp = foldr (+) 0

myConcat :: [[a]] -> [a]
myConcat = foldr (++) []

alwaysEven :: (Int -> Int) -> [Int] -> Bool
alwaysEven f xs = length (filter even (map f xs)) == length xs

--alwaysEven (+3) [7, 7, 9, 11]

updatePositivesOnly :: (Float -> Float) -> [Float] -> [Float]
updatePositivesOnly _ [] = []
updatePositivesOnly function (x : xs)
  | x > 0 = function x : updatePositivesOnly function xs
  | otherwise = x : updatePositivesOnly function xs


--1

mult10 :: [Int] -> [Int]
mult10 list = map (*10) list