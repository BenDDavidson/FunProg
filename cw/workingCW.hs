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

--iii printf "

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
demo 3 = do
  printf "%-10s %-10s %-10s %10s %10s\n" "Name" "North" "East" "This year" "Last year"
  putStrLn (formatCities testData) -- output the data as a string
demo 4 = do
  printf "%-10s %-10s %-10s %10s %10s\n" "Name" "North" "East" "This year" "Last year"
  putStrLn (formatCities (addPopulationData [1200,3200,3600,1800,9500,6800,11100,4300,2000,1800] testData))
demo 5 = do
  printf "%-10s %-10s %-10s %10s %10s\n" "Name" "North" "East" "This year" "Last year"
  putStrLn (formatCities (sortCitiesByName (addCity "Stockholm" 59 18 [1657, 1633, 1608, 1583] testData)))
demo 6 = print (getPopGrowth "Athens" testData)
demo 7 = putStrLn (closestCityToName 45 8 4000 testData)
demo _ = print "Invalid argument"


--
-- Your user interface (and loading/saving) code goes here
--

readPopulationData :: String -> [Double]
readPopulationData line = map read (words line)


lineToWordsList :: String -> [String]
lineToWordsList line = words line
{-

readCity :: [String] -> City
readCity (name:north:east:popList) = City name (CityCoordinates north east) readablePopList
  where readablePopList = readPopulationData popList


readCities :: [String] -> [City]
readCities [] = []
readCities (line:rest) = readCity splitLine : readCities rest
  where splitLine = lineToWordsList line

-}



main :: IO ()
main = do


  contents <- readFile "cities.txt"
  let linesOfFile = lines contents
  print linesOfFile



  putStrLn "Select one of the following options:"
  putStrLn "Enter a number 1-8 to execute the corresponding demo"
  putStrLn "1. The list of all city names"
  putStrLn "2. Return the population of a city at a given amount of years ago"
  putStrLn "3. Return the data as a formatted string"


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

