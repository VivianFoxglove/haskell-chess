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
        "RNB KBNR",
        "PPPPPPPP",
        "   q    ",
        "        ",
        "        ",
        "   Q    ",
        "pppppppp",
        "rnb kbnr"
    ]

newBoard :: Board
newBoard = map (map charToPiece) initialBoard

getPiece :: Board -> (Int, Int) -> Piece
getPiece board (x, y) = (board !! y) !! x

setRow :: [Piece] -> Int -> Piece -> [Piece]
setRow row y piece =
    take y row ++
    [piece] ++
    drop (y + 1) row

setPiece :: Board -> (Int, Int) -> Piece -> Board
setPiece board (x, y) piece =
    take y board ++
    [setRow (board !! y) x piece] ++
    drop (y + 1) board

movePiece :: Board -> (Int, Int) -> (Int, Int) -> IO Board
movePiece board (x1, y1) (x2, y2) = do
  piece <- pure $ getPiece board (x1, y1)
  return (setPiece removePiece (x2, y2) piece)
  where removePiece = setPiece board (x1, y1) Empty

getAllPieces :: Board -> [Piece]
getAllPieces board = concatMap (filter (/= Empty)) board

rowStr :: [Piece] -> String
rowStr row =
    "| "  ++ intercalate " | " (map (\x -> [pieceToChar x]) row) ++ " |"

boardStr :: Board -> String
boardStr board =
    let numberRows = map (\(x, y) -> show (x :: Integer) ++ " " ++ y) (zip [8,7..] (map rowStr board)) in
    "  +-------------------------------+\n" ++
    intercalate "\n  |---+---+---+---+---+---+---+---|\n" numberRows ++
    "\n  +-------------------------------+" ++
    "\n    A   B   C   D   E   F   G   H  "

printBoard :: Board -> IO ()
printBoard board = putStrLn $ boardStr board
