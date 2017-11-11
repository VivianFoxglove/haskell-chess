import Board
import Moves
import Piece

import Data.Char
import System.IO

moveToCoord :: String -> (Int, Int)
moveToCoord coord
    | length coord /= 2 = (-1, -1)
    | otherwise = (((ord (toUpper a)) - 65), 7 - ((ord b) - 49))
    where a = head coord
          b = last coord

readCoord :: IO (Int, Int)
readCoord = do
    alga <- getLine
    return $ moveToCoord alga

validMove :: Board -> Color -> (Int, Int) -> (Int, Int) -> Bool
validMove board color c1 c2
    | color /= targetColor = False
    | otherwise = any (== c2) (movesForPiece board c1)
    where targetColor = getColor $ getPiece board c1

inputMove :: Color -> Board -> IO (Color, Board)
inputMove color board = do
    putStr ((show color) ++ " to move (FROM): ")
    hFlush stdout
    c1 <- readCoord
    putStr ((show  color) ++ " to move (TO):   ")
    hFlush stdout
    c2 <- readCoord
    (if validMove board color c1 c2 then do
        nextBoard <- movePiece board c1 c2
        return (if color == White then Black else White, nextBoard)
    else do
        return (color, board)) >>= return

inputLoop :: Color -> Board -> IO ()
inputLoop color board = do
    printBoard board
    (nextColor, nextBoard) <- inputMove color board
    putStrLn "\x1b[2J\x1b[;H"
    if color == nextColor then
        putStrLn "ERROR: Invalid move"
    else
        return ()

    inputLoop nextColor nextBoard

main :: IO ()
main = do
    board <- pure newBoard
    putStrLn "\x1b[2J\x1b[;H"
    inputLoop White board
