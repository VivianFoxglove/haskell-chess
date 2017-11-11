module Piece
( PieceName(..)
, Color(..)
, Piece(..)
, charToPiece
, pieceToChar
, validPosition
) where

import Data.Char

data PieceName = Pawn | Knight | Bishop | Rook | Queen | King
    deriving (Show, Eq)
data Color = None | Black | White deriving (Show, Eq)
data Piece = Empty | Piece { pName :: PieceName, pColor :: Color }
    deriving (Show, Eq)

validPosition :: (Int, Int) -> Bool
validPosition (x, y) = x >= 0 && x < 8 && y >= 0 && y < 8

charToPiece :: Char -> Piece
charToPiece char =
    let color = if isLower char then White else Black in
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
    let method = if color == Black then toUpper else toLower in method $
    case name of
        Pawn   -> 'p'
        Knight -> 'n'
        Bishop -> 'b'
        Rook   -> 'r'
        Queen  -> 'q'
        King   -> 'k'
