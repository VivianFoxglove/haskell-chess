import Data.Char

data Coord =
    A1 | B1 | C1 | D1 | E1 | F1 | G1 | H1 |
    A2 | B2 | C2 | D2 | E2 | F2 | G2 | H2 |
    A3 | B3 | C3 | D3 | E3 | F3 | G3 | H3 |
    A4 | B4 | C4 | D4 | E4 | F4 | G4 | H4 |
    A5 | B5 | C5 | D5 | E5 | F5 | G5 | H5 |
    A6 | B6 | C6 | D6 | E6 | F6 | G6 | H6 |
    A7 | B7 | C7 | D7 | E7 | F7 | G7 | H7 |
    A8 | B8 | C8 | D8 | E8 | F8 | G8 | H8
    deriving (Show, Eq, Enum)

data PieceName = Pawn | Knight | Bishop | Rook | Queen | King
    deriving (Show, Eq)
data Color = Black | White deriving (Show, Eq)
data Piece = Empty | Piece { pName :: PieceName, pColor :: Color }
    deriving (Show, Eq)

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

getPiece :: Board -> Coord -> Piece
getPiece board coord =
    let rank = (fromEnum coord) `div` 8
        file = (fromEnum coord) `mod` 8 in
    board !! rank !! file

getAllPieces :: Board -> [Piece]
getAllPieces board = concatMap (filter (/= Empty)) board

charToPiece :: Char -> Piece
charToPiece char =
    let color = if isLower char then Black else White in
    case toLower char of
        'p' -> Piece { pName = Pawn,    pColor = color }
        'n' -> Piece { pName = Knight,  pColor = color }
        'b' -> Piece { pName = Bishop,  pColor = color }
        'r' -> Piece { pName = Rook,    pColor = color }
        'q' -> Piece { pName = Queen,   pColor = color }
        'k' -> Piece { pName = King,    pColor = color }
        _   -> Empty


pieceToChar :: Piece -> Char
pieceToChar Empty = ' '
pieceToChar (Piece name color) =
    let method = if color == Black then toLower else toUpper in method $
    case name of
        Pawn   -> 'p'
        Knight -> 'n'
        Bishop -> 'b'
        Rook   -> 'r'
        Queen  -> 'q'
        King   -> 'k'

printBoard :: Board -> IO ()
printBoard board = do
    putStrLn $ unlines $ map (map pieceToChar) board

main :: IO ()
main = do
    board <- pure newBoard
    print $ getPiece board E1
    printBoard board
