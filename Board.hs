module Board
( newBoard
, setPiece
, getPiece
, getAllPieces
, printBoard
) where

import Piece

type Board = [[Piece]]

initialBoard :: [String]
initialBoard =
    [
        "RNBQKBNR",
        "PPPPPPPP",
        "        ",
        "        ",
        "        ",
        "        ",
        "pppppppp",
        "rnbqkbnr"
    ]

newBoard :: Board
newBoard = map (map charToPiece) initialBoard

getPiece :: Board -> (Int, Int) -> Piece
getPiece board (file, rank) = board !! rank !! file

setRow :: [Piece] -> Int -> Piece -> [Piece]
setRow row file piece =
    take file row ++
    [piece] ++
    drop (file + 1) row

setPiece :: Board -> (Int, Int) -> Piece -> Board
setPiece board (file, rank) piece =
    take rank board ++
    [setRow (board !! rank) file piece] ++
    drop (rank + 1) board

getAllPieces :: Board -> [Piece]
getAllPieces board = concatMap (filter (/= Empty)) board

printBoard :: Board -> IO ()
printBoard board = do
    putStrLn $ unlines $ reverse $ map (map pieceToChar) board
