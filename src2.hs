module Chess where

import Data.Char

type Board = [[Square]]

initialBoardStr :: String
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

readBoard :: String -> Maybe Board
readBoard = (mapM . mapM) readSquare . lines

showBoard :: Board -> String
showBoard = unlines . (map . map) showSquare

type Square = Maybe Piece

showSquare :: Square -> Char
showSquare = maybe ' ' showPiece

readSquare :: Char -> Maybe Square
readSquare '.' = Just Nothing
readSquare c = fmap Just (readPiece c)

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

typeList :: [(Char, PType)]
typeList = [('p', Pawn)
           ,('h', Horse)
           ,('b', Bishop)
           ,('r', Rook)
           ,('q', Queen)
           ,('k', King)
           ]

readPiece :: Char -> Maybe Piece
readPiece c = fmap makePiece lookupType
  where color = if isUpper c then White else Black
        lookupType = lookup (toLower c) typeList
        makePiece = Piece color
