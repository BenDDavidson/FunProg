--
-- MATHFUN
-- My Haskell assignment program
-- UP2025489
--

import Text.Printf

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


getCityByName :: String -> [City] -> Maybe City
getCityByName _ [] = Nothing
getCityByName name (city:rest) =
  if name == cityName city
    then Just city
    else getCityByName name rest

getPopList :: City -> [cityPopulationList]
getPopList city = cityPopulationList city



--
--  Demo
--
demo :: Int -> IO ()
demo 1 = print (cityNames testData) -- output the names of all the cities

demo _ = print "Invalid argument"


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

main :: IO ()
main = do
  putStrLn "Select one of the following options:"
  putStrLn "Enter a number 1-8 to execute the corresponding demo"
  putStrLn "Press any other key to exit"
  option <- getLine
  case option of
    "1" -> do
      putStrLn "The list of all city names:"
      demo 1
      main
    "2" -> do
      putStrLn "The population of Berlin 1 year ago:"
      main
    "3" -> do
      putStrLn "The data as a string:"
      main
    "4" -> do
      putStrLn "The data after it has been updated:"
      main
    "5" -> do
      putStrLn "The data after Stockholm has been added:"
      main
    "6" -> do
      putStrLn "The annual growth figures for Athens:"
      main
    "7" -> do
      putStrLn "The nearest city to location (45N, 8E) with a population above 4m people:"
      main
    "8" -> do
      putStrLn "The population map:"
      main
    _ -> return ()

