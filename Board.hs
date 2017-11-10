module Board
( Board
, newBoard
, setPiece
, movePiece
, getPiece
, getAllPieces
, printBoard
) where

import Data.List
import Piece

type Board = [[Piece]]

initialBoard :: [String]
initialBoard =
    [
        "RNBQKBNR",
        "PPPPPPPP",
        "        ",
        "        ",
        "q       ",
        "        ",
        "pppppppp",
        "rnb kbnr"
    ]

newBoard :: Board
newBoard = map (map charToPiece) initialBoard

getPiece :: Board -> (Int, Int) -> Piece
getPiece board (x, y) = (board !! (7 - y)) !! x

setRow :: [Piece] -> Int -> Piece -> [Piece]
setRow row y piece =
    take y row ++
    [piece] ++
    drop (y + 1) row

setPiece :: Board -> (Int, Int) -> Piece -> Board
setPiece board (x, y_) piece =
    let y = 7 - y_ in
    take y board ++
    [setRow (board !! y) x piece] ++
    drop (y + 1) board

movePiece :: Board -> (Int, Int) -> (Int, Int) -> IO Board
movePiece board (x1, y1) (x2, y2) = do
  piece <- pure $ getPiece board (x1, y1)
  board <- pure $ setPiece board (x1, y1) Empty
  board <- pure $ setPiece board (x2, y2) piece
  return board

getAllPieces :: Board -> [Piece]
getAllPieces board = concatMap (filter (/= Empty)) board

rowStr :: [Piece] -> String
rowStr row =
    "| "  ++ intercalate " | " (map (\x -> [pieceToChar x]) row) ++ " |"

boardStr :: Board -> String
boardStr board =
    let numberRows board = map (\(x, y) -> show x ++ " " ++ y) (zip [8,7..] (map rowStr board)) in
    "  +-------------------------------+\n" ++
    intercalate "\n  |---+---+---+---+---+---+---+---|\n" (numberRows board) ++
    "\n  +-------------------------------+" ++
    "\n    A   B   C   D   E   F   G   H  "

printBoard :: Board -> IO ()
printBoard board = putStrLn $ boardStr board
