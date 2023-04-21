{- Week5.hs
 This file illustrates list patterns and recursion over lists.
-}

import Prelude hiding (concat, fst, head, reverse, snd, sum, tail, zip)

-- Definitions of the prelude functions fst and snd

fst (x, _) = x

snd (_, y) = y

-- Definitions of the prelude functions head and tail

head :: [p] -> p
head (x : _) = x
head [] = error "head: empty list"

tail :: [a] -> [a]
tail (_ : xs) = xs
tail [] = error "tail: empty list"

absFirst :: [Int] -> Int
absFirst [] = -1
absFirst (x : xs) = abs x

sum :: [Int] -> Int
sum [] = 0
sum (x : xs) = x + sum xs

doubleAll :: [Int] -> [Int]
doubleAll [] = []
doubleAll (x : xs) = 2 * x : doubleAll xs

concat :: [[a]] -> [a]
concat [] = []
concat (x : xs) = x ++ concat xs

reverse :: [a] -> [a]
reverse [] = []
reverse (x : xs) = reverse xs ++ [x]

zip :: [a] -> [b] -> [(a, b)]
zip (x : xs) (y : ys) = (x, y) : zip xs ys
zip _ _ = []

-- For question 10
type StudentMark = (String, Int)

testData :: [StudentMark]
testData =
  [ ("John", 53),
    ("Sam", 16),
    ("Kate", 85),
    ("Jill", 65),
    ("Bill", 37),
    ("Amy", 22),
    ("Jack", 41),
    ("Sue", 71)
  ]

countSpaces :: String -> Int
--countSpaces "" = 0
--countSpaces (x:xs)
--  | x == ' ' = 1 + countSpaces xs
--  | otherwise = countSpaces xs
countSpaces str = sum [1 | x <- str, x == ' ']

mergeLists :: [Int] -> [Int] -> [Int]
mergeLists [] [] = []
mergeLists [] ys = ys
mergeLists xs [] = xs
mergeLists (x : xs) (y : ys)
  | x <= y = x : mergeLists xs (y : ys)
  | otherwise = y : mergeLists (x : xs) ys

--skipped above

--1

headPlusOne :: [Int] -> Int
headPlusOne [] = -1
headPlusOne (x : xs) = x + 1

removeSecond :: [a] -> [a]
removeSecond [] = []
removeSecond [x] = [x]
removeSecond (x : y : xs) = x : xs

--2

duplicateHead :: [a] -> [a]
duplicateHead [] = []
duplicateHead (x : xs) = x : x : xs

--3

rotate :: [a] -> [a]
rotate [] = []
rotate [x] = [x]
rotate (x : y : xs) = y : x : xs

--4

listLength :: [a] -> Int
listLength [] = 0
listLength (x : xs) = 1 + listLength xs

--5

multAll :: [Int] -> Int
multAll [] = 1
multAll (x : xs) = x * multAll xs

--6