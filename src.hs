type Board = [[Square]]

initialBoardStr = unlines [
                           "rhbqkbhr"
                          ,"pppppppp"
                          ,"        "
                          ,"        "
                          ,"        "
                          ,"        "
                          ,"PPPPPPPP"
                          ,"RHBQKBHR"
                          ]

readBoard :: String -> Board
readBoard = map readRow . lines
  where readRow = map readSquare

showBoard :: Board -> String
showBoard = unlines . map showRow
  where showRow = map showSquare

type Square = Maybe Piece

showSquare :: Square -> Char
showSquare = maybe ' ' showPiece

readSquare :: Char -> Square
readSquare ' ' = Nothing
readSquare c = Just (readPiece c)

data Piece = Piece PColor PType deriving (Show)
data PColor = White | Black deriving (Show)
data PType = Pawn | Horse | Bishop | Rook | Queen | King deriving (Show)

showPiece :: Piece -> Char
showPiece (Piece White Pawn)   = 'P'
showPiece (Piece White Horse)  = 'H'
showPiece (Piece White Bishop) = 'B'
showPiece (Piece White Rook)   = 'R'
showPiece (Piece White Queen)  = 'Q'
showPiece (Piece White King)   = 'K'
showPiece (Piece Black Pawn)   = 'p'
showPiece (Piece Black Horse)  = 'h'
showPiece (Piece Black Bishop) = 'b'
showPiece (Piece Black Rook)   = 'r'
showPiece (Piece Black Queen)  = 'q'
showPiece (Piece Black King)   = 'k'

readPiece :: Char -> Piece
readPiece 'P' = (Piece White Pawn)
readPiece 'H' = (Piece White Horse)
readPiece 'B' = (Piece White Bishop)
readPiece 'R' = (Piece White Rook)
readPiece 'Q' = (Piece White Queen)
readPiece 'K' = (Piece White King)
readPiece 'p' = (Piece Black Pawn)
readPiece 'h' = (Piece Black Horse)
readPiece 'b' = (Piece Black Bishop)
readPiece 'r' = (Piece Black Rook)
readPiece 'q' = (Piece Black Queen)
readPiece 'k' = (Piece Black King)

