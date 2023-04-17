import Data.Char

type StudentMark = (String, Int)

betterStudent :: StudentMark -> StudentMark -> String
betterStudent (s1, m1) (s2, m2)
  | m1 >= m2 = s1
  | otherwise = s2

marks :: [StudentMark] -> [Int]
marks stmks = [mk | (st, mk) <- stmks]

pass :: [StudentMark] -> [String]
pass stmks = [st | (st, mk) <- stmks, mk >= 40]

-- An example list of student marks
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

addPairs :: [(Int, Int)] -> [Int]
addPairs pairList = [i + j | (i, j) <- pairList]

minAndMax :: Int -> Int -> (Int, Int)
minAndMax x y
  | x <= y = (x, y)
  | otherwise = (y, x)

sumEvenNumbersBetween :: Int -> Int -> Int
sumEvenNumbersBetween x y = sum [i | i <- [x .. y], even i]


--sumEvenNumbersBetween x y
--  | x > y = 0
--  | otherwise = x + sumEvenNumbersBetween (x + 1) y


averageMark :: [StudentMark] -> Float
averageMark [] = 0
averageMark stmks = fromIntegral sumMarks / fromIntegral numberOfStudents
  where
    sumMarks = sum [mk | (_ , mk) <- stmks]
    numberOfStudents = length stmks


--1

sumDifference :: Int -> Int -> (Int, Int)
sumDifference x y = (x+y,x-y)

--2

grade :: StudentMark -> Char
grade (st, mk)
  | mk >= 70 = 'A'
  | mk >= 60 = 'B'
  | mk >= 50 = 'C'
  | mk >= 40 = 'D'
  | otherwise = 'F'

--3

capMark :: StudentMark -> StudentMark
capMark (st, mk)
  | mk > 40 = (st, 40)
  | otherwise = (st, mk)

--4

firstNumbers :: Int -> [Int]
firstNumbers n = [1..n]

--5

firstSquares :: Int -> [Int]
firstSquares n = [x ^ 2 | x <- firstNumbers n]

--6

capitalise :: String -> String
capitalise input = [toUpper i | i <- input]

--7

onlyDigits :: String -> String
onlyDigits input = [toUpper i | i <- input, isDigit i]
  where isDigit ch = ch `elem` ['0'..'9']

--8

capMarks :: [StudentMark] -> [StudentMark]
capMarks inputList = [capMark i | i <- inputList]

--9

gradeStudents :: [StudentMark] -> [(String, Char)]
gradeStudents inputList = [grade mark | mark <- marksList]
  where marksList = marks inputList

