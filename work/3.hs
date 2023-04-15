-- We don't import '||' from the prelude, so that we can
-- define our own version

import Prelude hiding ((||), (&&))

-- The following line declares the || operator (which we are about to
-- re-define) to be right associative and to have precedence 2. This
-- is necessary in order for expressions such as False || x > 2 to be
-- valid (e.g. it sets the precedence of || to be lower than >).

infixr 2 ||
infixr 3 &&

-- A naive re-implementation of the Prelude operator ||
(||) :: Bool -> Bool -> Bool
True || True = True
False || True = True
True || False = True
False || False = False

-- An alternative re-implementation
--(||) :: Bool -> Bool -> Bool
--False || False   = False
--_ || _           = True

-- Another alternative re-implementation
--(||) :: Bool -> Bool -> Bool
--True || _     =  True
--False || a    = a

fact :: Int -> Int
fact n
  | n == 0 = 1
  | n > 0 = n * fact (n - 1)
  | otherwise = error "factorials not defined for negative ints"

mult :: Int -> Int -> Int
mult n m
  | n == 0 = 0
  | n > 0 = m + mult (n - 1) m
  | otherwise = - mult (- n) m

divide :: Int -> Int -> Int
divide n m
  | n < m = 0
  | otherwise = 1 + divide (n - m) m

--fibonacci :: Int -> Int
--fibonacci n
--  | n == 0 = 0
--  | n == 1 = 1
--  | otherwise = fibonacci (n - 1) + fibonacci (n - 2)

fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n - 1) + fibonacci (n - 2)

--1

--(&&) :: Bool -> Bool -> Bool
--True && True = True
--False && True = False
--True && False = False
--False && False = False

-- simplified

(&&) :: Bool -> Bool -> Bool
True && True = True
_ && _ = False

-- alternative

--(&&) :: Bool -> Bool -> Bool
--True && b = b
--False && _ = False

--2

myXor :: Bool -> Bool -> Bool
myXor a b = (a /= b)

--3

ifThenElse :: Bool -> int -> int -> int
ifThenElse True x y = x
ifThenElse False x y = y

--4

--daysInMonth :: Int -> Int
--daysInMonth month
--daysInMonth month
--  where
--    divisibleBy x y = x `mod` y == 0


validDate :: Int -> Int -> Bool
validDate day month
  | month < 1 || month > 12 = False
  | month `elem` [1, 3, 5, 7, 8, 10, 12] = day >= 1 && day <= 31
  | month `elem` [4, 6, 9, 11] = day >= 1 && day <= 30
  | month == 2 = day >= 1 && day <= 28
  | otherwise = False

--5

sumNumbers :: Int -> Int
sumNumbers n
  | n == 0 = 0
  | otherwise = sumNumbers (n-1) + n

--6

sumSquares :: Int -> Int
sumSquares n
  | n == 0 = 0
  | otherwise = sumSquares (n-1) + n ^ 2

--7

power :: Int -> Int -> Int
power n m
  |