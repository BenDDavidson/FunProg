--
-- MATHFUN
-- My Haskell assignment program
-- UP2025489
--

import Text.Printf
import Data.List

--
-- Types (define City type here)
--

type CityName = String
type North = Int
type East = Int
type CityPopulationList = [Double]

data CityCoordinates = CityCoordinates North East
  deriving (Show)

data City = City CityName CityCoordinates CityPopulationList
  deriving (Show)


testData :: [City]
testData =
  [ City "Amsterdam" (CityCoordinates 52 5) [1158, 1149, 1140, 1132]
  , City "Athens" (CityCoordinates 38 24) [3153, 3153, 3154, 3156]
  , City "Berlin" (CityCoordinates 53 13) [3567, 3562, 3557, 3552]
  , City "Bucharest" (CityCoordinates 44 26) [1794, 1803, 1812, 1821]
  , City "London" (CityCoordinates 52 0) [9426, 9304, 9177, 9046]
  , City "Madrid" (CityCoordinates 40 4) [6669, 6618, 6559, 6497]
  , City "Paris" (CityCoordinates 49 2) [11079, 11017, 10958, 10901]
  , City "Rome" (CityCoordinates 42 13) [4278, 4257, 4234, 4210]
  , City "Vienna" (CityCoordinates 48 16) [1945, 1930, 1915, 1901]
  , City "Warsaw" (CityCoordinates 52 21) [1790, 1783, 1776, 1768]
  ]



--
--  Your functional code goes here
--

cityNames :: [City] -> [CityName]
cityNames [] = []
cityNames (City name _ _ : cities) = name : cityNames cities

--i

getCityName :: City -> CityName
getCityName (City cityName _ _) = cityName

--ii

findCityByName :: String -> [City] -> City
findCityByName name (city:rest) =
  if name == getCityName city
    then city
    else findCityByName name rest

getPopList :: City -> [Double]
getPopList (City _ _ popList) = popList

getPopAtYearsAgo :: CityName -> Int -> [City] -> String
getPopAtYearsAgo name yearsAgo cities =
  printf "The population of %s %d years ago was %.3fm"
    name yearsAgo (pop / 1000.0)
  where
    city = findCityByName name cities
    popList = getPopList city
    pop = popList !! yearsAgo

--iii

formatCity :: City -> String
formatCity (City name (CityCoordinates north east) popList) =
  printf "%-10s %-10d %-10d %10.3fm %10.3fm" name north east (popList !! 0 / 1000.0) (popList !! 1 / 1000.0)

formatCities :: [City] -> String
formatCities [] = ""
formatCities (city:rest) = formatCity city ++ "\n" ++ formatCities rest

--iv

updatePopulation :: Double -> City -> City
updatePopulation newPop (City cityName coords oldPopList) =
  City cityName coords (newPop : oldPopList)

addPopulationData :: [Double] -> [City] -> [City]
addPopulationData [] cities = cities
addPopulationData (pop:restOfpop) (city:restOfCities) =
  updatedCity : addPopulationData restOfpop restOfCities
  where
    updatedCity = updatePopulation pop city

--v

addCity :: CityName -> North -> East -> CityPopulationList -> [City] -> [City]
addCity name north east popList cities = City name (CityCoordinates north east) popList : cities

-- USAGE testDataWithStockholm = addCity "Stockholm" 59 18 [1657, 1633, 1608, 1583] testData


sortCitiesByName :: [City] -> [City]
sortCitiesByName cities = sortBy compareNames cities
  where
    compareNames city1 city2 = compare (getCityName city1) (getCityName city2)

--vi

popDifference :: [Double] -> [Double]
popDifference [] = []
popDifference (pop:[]) = []
popDifference (pop1:pop2:rest) = (pop1 - pop2) : popDifference (pop2:rest)

getPopGrowth :: CityName -> [City] -> [Double]
getPopGrowth name cities = popDifference popList
  where
    city = findCityByName name cities
    popList = getPopList city

--vii

citiesMeetingMin :: Int -> [City] -> [City]
citiesMeetingMin minPop cities = [city | city <- cities, (head (getPopList city)) >= fromIntegral minPop]
  where
    popList = getPopList (head cities)

getNorth :: City -> North
getNorth (City _ (CityCoordinates north _) _) = north

getEast :: City -> East
getEast (City _ (CityCoordinates _ east) _) = east

findClosestCity :: North -> East -> Int -> [City] -> City
findClosestCity north east minPop cities = head (sortBy compareDistances citiesMeeting)
  where
    compareDistances city1 city2 = compare (distance city1) (distance city2)
    distance city = sqrt (fromIntegral (north - getNorth city) ^ 2 + fromIntegral (east - getEast city) ^ 2)
    citiesMeeting = citiesMeetingMin minPop cities


closestCityToName :: North -> East -> Int -> [City] -> String
closestCityToName north east minPop cities = printf "The closest city to %d,%d with a population of at least %d is %s"
  north east minPop cityName
  where
    closestCity = findClosestCity north east minPop cities
    cityName = getCityName closestCity


--
--  Demo
--
demo :: Int -> IO ()
demo 1 = print (cityNames testData) -- output the names of all the cities
demo 2 = print (getPopAtYearsAgo "Berlin" 1 testData) -- output the population of Berlin 1 year ago
demo 3 = putStrLn (formatCities testData) -- output the data as a string
demo 4 = putStrLn (formatCities (addPopulationData [1200,3200,3600,1800,9500,6800,11100,4300,2000,1800] testData))
demo 5 = putStrLn (formatCities (sortCitiesByName (addCity "Stockholm" 59 18 [1657, 1633, 1608, 1583] testData)))
demo 6 = print (getPopGrowth "Athens" testData)
demo _ = print "Invalid argument"
--todo add headings to formatter
--todo make demos interactive, look at 9.hs

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

{-
demo 1 = -- output the names of all the cities
demo 2 = -- output the population of "Berlin" 1 year ago (i.e. last year)
demo 3 = putStrLn (citiesToString testData)
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

