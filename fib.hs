fib 0 = 1
fib 1 = 1
fib x = fib (x - 2) + fib (x - 1)

main = do
  putStr "Which Fibonacci number do you want? "
  n <- getLine
  let nInt = read n::Int
  print $ fib nInt
