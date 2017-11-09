module Moves
( getColor
, rookMoves
, spotAvailable
, spotAvailableForPiece
, knightMoves
, queenMoves
) where

import Board
import Piece

getColor (Piece _ color) = color
getColor Empty = None

isOutOfBounds (x1, y1) =
    x1 < 0 || x1 >= 8 || y1 < 0 || y1 >= 8

spotAvailable board (x1, y1) (x2, y2) =
    let p1 = getPiece board (x1, y1)
        p2 = getPiece board (x2, y2) in
    if      isOutOfBounds (x1, y1) then False
    else if isOutOfBounds (x2, y2) then False
    else if p1 == Empty then False
    else   (getColor p1) /= (getColor p2)

spotAvailableForPiece :: Board -> Piece -> (Int, Int) -> Bool
spotAvailableForPiece board (Piece _ color) (x, y) =
    let piece = getPiece board (x, y) in
    if      isOutOfBounds (x, y) then False
    else if color == None then False
    else    color /= (getColor piece)
spotAvailableForPiece board Empty (x, y) = False

isCollision board (Piece _ color) (x, y) =
    let targetColor = getColor $ getPiece board (x, y) in
    targetColor /= None && color /= targetColor

moveUntilCollision board piece ((x, y):xs)
    | isOutOfBounds (x, y) = []
    | isCollision board piece (x, y) = [(x, y)]
    | spotAvailableForPiece board piece (x, y) = [(x, y)] ++ moveUntilCollision board piece xs
    | otherwise = []

rookMoves board (x, y) =
    let piece = getPiece board (x, y) in
    concatMap (moveUntilCollision board piece)
        [
            [(x + n, y) | n <- [1..8]],
            [(x - n, y) | n <- [1..8]],
            [(x, y + n) | n <- [1..8]],
            [(x, y - n) | n <- [1..8]]
        ]

knightMoves board (x, y) =
    let piece = getPiece board (x, y) in
    filter (spotAvailableForPiece board piece)
        [(x + 2, y - 1), (x + 2, y + 1), (x - 2, y - 1), (x - 2, y + 1),
         (x + 1, y - 2), (x + 1, y + 2), (x - 1, y - 2), (x - 1, y + 2)]

bishopMoves board (x, y) =
    let piece = getPiece board (x, y) in
    concatMap (moveUntilCollision board piece)
        [
            [(x + n, y + n) | n <- [1..8]],
            [(x + n, y - n) | n <- [1..8]],
            [(x - n, y + n) | n <- [1..8]],
            [(x - n, y - n) | n <- [1..8]]
        ]

queenMoves board (x, y) =
    rookMoves board (x, y) ++ bishopMoves board (x, y)
