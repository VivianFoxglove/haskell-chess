import Data.Array.IO

type Board = IO (IOArray Int Int)

newBoard = newArray (0, 64) 0 :: Board

getXY board (x, y) = do
    pos <- readArray board (y * 8 + x)
    return pos

main = do
    board <- newBoard
    pos <- getXY board (0, 0)
    print $ pos
