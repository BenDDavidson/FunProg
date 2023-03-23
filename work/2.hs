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
absolute x = if x < 0 then -x else x