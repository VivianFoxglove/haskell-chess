module Board
( newBoard
, setPiece
, movePiece
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

movePiece board (f1, r1) (f2, r2) = do
  piece <- pure $ getPiece board (f1, r1)
  board <- pure $ setPiece board (f1, r1) Empty
  board <- pure $ setPiece board (f2, r2) piece
  return board

getAllPieces :: Board -> [Piece]
getAllPieces board = concatMap (filter (/= Empty)) board

printBoard :: Board -> IO ()
printBoard board = do
    putStrLn $ unlines $ reverse $ map (map pieceToChar) board
