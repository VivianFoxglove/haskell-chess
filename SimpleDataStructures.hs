exampleList = [1,2,3,4,5]
exampleListOp1 = [1..100]
tupleTest = (1,"one")
tupleList = [(1,"one"),(2,"two"),(3,"three")]

main = do 
   putStrLn "Hello, this Haskell program will demonstrate\nsome of the major data structures of the language.\n"
   putStrLn "Here is a list, one of the most common data structures:"
   print exampleList
   
   putStrLn "\nlets do some operations on this list by calling some functions.\n"
   
   putStrLn "Here we call the head function to return the first element:"
   print (head exampleList)
   putStrLn "What is the length of this list?"
   print (length exampleList)

   putStrLn "\nHere's another data strcture called a tuple:"
   print tupleTest
   putStrLn "\nLets combine, here is a list of tuples:"
   print tupleList
   
   putStrLn  "\n Heres a list comprehension of range and their odds numbers"
   print [x | x <- exampleListOp1 , x `mod` 2 == 1]
