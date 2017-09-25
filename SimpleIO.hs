testEq :: Char -> String
testEq x =
    if x == 'm' || x == 'n'
    then "You're good at following directions!\n"
    else "At least you got this far.\n"

main = do  
   putStrLn "Here we will go over some basic concepts of I/O in Haskell."
   putStrLn "So, what is your name?"
   fName <- getLine
   putStrLn ("Hey " ++ fName ++ " im glad you are here to see some Haskell I/O!\n")

   putStrLn "We will now demonstrate reading from an external file.\n"


   putStrLn "In Haskell, we read from a text file and print it here:"
   let file = "readOPS.txt" 
   readIn <- readFile file 
   putStrLn readIn
   putStrLn "\nNow we are back to the main file.\n"
   
   putStrLn "Lets do a sequence, enter three strings, letters, or numbers, pressing enter after each."
   testSequence <- sequence [getLine, getLine, getLine]  
   putStr "\nHere is your sequence:\n" 
   print testSequence
   
   putStrLn "\nLastly lets check some equality, input 'n' or 'm'."
   letterEq <- getChar
   putStrLn $ testEq letterEq
   putStrLn "This is the end of the I/O program, goodbye!"
