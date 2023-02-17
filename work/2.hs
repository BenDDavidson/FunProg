numMultsOf10 :: Int -> Int -> Int
numMultsOf10 x y = isMultiple x + isMultiple y
  where
    isMultiple z = if z `mod` 10 == 0 then 1 else 0

