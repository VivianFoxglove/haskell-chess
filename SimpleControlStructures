import System.IO
import Text.Read
import Data.Maybe

fib 0 = 1
fib 1 = 1
fib x = fib (x - 2) + fib (x - 1)

testChar :: Char -> String
testChar x =
  if x >= 'a' && x <= 'z'
  then "This is a lowercase letter."
  else if x >= 'A' && x <= 'Z'
  then "This is an uppercase letter."
  else "This is not a letter!"

testInt :: String -> String
testInt x = do
  let maybeInt = readMaybe x :: Maybe Int
  if isJust maybeInt
  then "This is an integer!"
  else "This is not an integer."


main = do
  hSetBuffering stdin NoBuffering

  putStrLn "Here we will go over some basic control structures in Haskell."
  putStr "Enter a letter and we will perform some tests on it: "
  letterTest <- getChar
  putStrLn $ "\n" ++ (testChar letterTest)

  putStrLn "\nPlease enter a number: "
  numberTest <- getLine
  putStrLn $ testInt numberTest

  putStr "\nWhich Fibonacci number do you want? "
  n <- getLine
  let nInt = read n::Int
  print $ fib nInt
