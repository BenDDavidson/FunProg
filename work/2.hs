heartMonitor :: Int -> Int -> String
heartMonitor age bpm
  | age > 80 && bpm > 100 = "High heart rate for +80!"
  | age > 60 && bpm > 130 = "High heart rate for 60-80!"
  | age > 40 && bpm > 140 = "High heart rate for 40-60!"
  | age > 20 && bpm > 155 = "High heart rate for 20-40!"
  | age >= 0 && bpm > 170 = "High heart rate for 0-20!"
  | otherwise = "Normal heart rate"

pizzaCalories :: Int -> String -> Float
pizzaCalories diameter toppings
  | toppings == "pepperoni" = (11.5 + 6) * pi * (fromIntegral diameter / 2) ^ 2
  | toppings == "tuna" = (11.5 + 4) * pi * (fromIntegral diameter / 2) ^ 2
  | toppings == "veggie" = (11.5 + 2.5) * pi * (fromIntegral diameter / 2) ^ 2
  | otherwise = 11.5 * pi * (fromIntegral diameter / 2) ^ 2

--1

absolute :: Int -> Int
absolute x
  | x < 0 = -x
  | otherwise = x

--2

sign :: Int -> Int
sign x
  | x < 0 = -1
  | x > 0 = 1
  | otherwise = 0

--3

howManyEqual :: Int -> Int -> Int -> Int
howManyEqual a b c
  | a == b && b == c = 3
  | a == b || b == c || a == c = 2
  | otherwise = 0

--4

x :: Float -> Float -> Float -> Float
x a b c = diagLength a + diagLength b + diagLength c
  where
    diagLength x = sqrt (x ^ 2 + x ^ 2)




sumDiagonalLengths :: Float -> Float -> Float -> Float