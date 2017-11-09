module Board
( Board
, newBoard
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

-- getColor (Piece _ color) = color

movePiece board (x1, y1) (x2, y2) = do
  piece <- pure $ getPiece board (x1, y1)
  board <- pure $ setPiece board (x1, y1) Empty
  board <- pure $ setPiece board (x2, y2) piece
  return board

getAllPieces :: Board -> [Piece]
getAllPieces board = concatMap (filter (/= Empty)) board

printBoard :: Board -> IO ()
printBoard board = do
    putStrLn $ unlines $ map (map pieceToChar) board
