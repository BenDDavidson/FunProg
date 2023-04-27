--
-- MATHFUN
-- My Haskell assignment program
-- UP2025489
--


--
-- Types (define City type here)
--


data City = City
  { cityName :: String
  , cityCoordinates :: (Int, Int)
  , cityPopulationList :: [Int]
  } deriving (Show, Eq)


testData :: [City]
testData =
  [ City "Amsterdam" (52, 5) [1158, 1149, 1140, 1132]
  , City "Athens" (38, 24) [3153, 3153, 3154, 3156]
  , City "Berlin" (53, 13) [3567, 3562, 3557, 3552]
  , City "Bucharest" (44, 26) [1794, 1803, 1812, 1821]
  , City "London" (52, 0) [9426, 9304, 9177, 9046]
  , City "Madrid" (40, 4) [6669, 6618, 6559, 6497]
  , City "Paris" (49, 2) [11079, 11017, 10958, 10901]
  , City "Rome" (42, 13) [4278, 4257, 4234, 4210]
  , City "Vienna" (48, 16) [1945, 1930, 1915, 1901]
  , City "Warsaw" (52, 21) [1790, 1783, 1776, 1768]
  ]


--
--  Your functional code goes here
--

cityNames :: [City] -> [String]
cityNames cities = map cityName cities




--
--  Demo
--
demo :: Int -> IO ()
demo 1 = print (cityNames testData) -- output the names of all the cities
demo _ = putStrLn "Invalid argument"

{-

demo :: Int -> IO ()
demo 1 = cityNames testData -- output the names of all the cities
demo 2 = -- output the population of "Berlin" 1 year ago (i.e. last year)
demo 3 = -- putStrLn (citiesToString testData)
demo 4 = -- output the data (as for (iii)) after it has been updated with the
         -- following new population figures (the first is for Amsterdam, etc.)
         -- [1200,3200,3600,1800,9500,6800,11100,4300,2000,1800]
demo 5 = -- show the data (as for (iii)) after adding "Stockholm" (59N, 18E)
         -- with population figures [1657, 1633, 1608, 1583]
demo 6 = -- output a list of annual growth figures for "Athens"
demo 7 = -- output the nearest city to location (45N, 8E) with
         -- a population above 4m people
demo 8 = -- output the population map

-}




--
-- Screen Utilities (use these to do the population map)
--

type ScreenPosition = (Int,Int)

-- Clears the screen
clearScreen :: IO ()
clearScreen = putStr "\ESC[2J"

-- Moves to a position on the screen
goTo :: ScreenPosition -> IO ()
goTo (x, y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

-- Writes a string at a position on the screen
writeAt :: ScreenPosition -> String -> IO ()
writeAt position text = do
    goTo position
    putStr text
 

--
-- Your population map code goes here
--



--
-- Your user interface (and loading/saving) code goes here
--
 
