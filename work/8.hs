helloWorld :: IO ()
helloWorld = putStrLn "Hello, World!"

displayFile :: IO ()
displayFile = do
    putStr "Enter the filename: "
    name <- getLine
    contents <- readFile name
    putStr contents

getInt :: IO Int
getInt = do
    str <- getLine
    return (read str :: Int)

isPalindrome :: String -> String
isPalindrome str
   | str == reverse str  = str ++ " is a palindrome"
   | otherwise           = str ++ " is not a palindrome"

pal :: IO ()
pal = do
    line <- getLine
    let response = isPalindrome line
    putStrLn response

palLines :: IO ()
palLines = do
    putStr "Enter a line: "
    str <- getLine
    if str == "" then
        return ()
    else do
        putStrLn (isPalindrome str)
        palLines

guessingGame :: Int -> IO ()
guessingGame secret =  do
  putStrLn "Enter a number: "
  guess <- getLine
  let guessNum = read guess :: Int
  if guessNum == secret then
    putStrLn "You win!"
  else do
    if guessNum > secret then
      putStrLn "Too high!"
    else
      putStrLn "Too low!"
    guessingGame secret

testCities = ["London", "Paris", "New York", "Tokyo", "Sydney"]

