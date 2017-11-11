module Moves
( getColor
, movesForPiece
, rookMoves
, spotAvailable
, spotAvailableForPiece
, knightMoves
, queenMoves
) where

import Board
import Piece

getColor :: Piece -> Color
getColor (Piece _ color) = color
getColor Empty = None

isOutOfBounds :: (Int, Int) -> Bool
isOutOfBounds (x1, y1) =
    x1 < 0 || x1 >= 8 || y1 < 0 || y1 >= 8

spotAvailable :: Board -> (Int, Int) -> (Int, Int) -> Bool
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
spotAvailableForPiece _ Empty (_, _) = False

isCollision :: Board -> Piece -> (Int, Int) -> Bool
isCollision _ Empty (_, _) = True
isCollision board (Piece _ color) (x, y) =
    let targetColor = getColor $ getPiece board (x, y) in
    targetColor /= None && color /= targetColor

moveUntilCollision :: Board -> Piece -> [(Int, Int)] -> [(Int, Int)]
moveUntilCollision _ _ [] = []
moveUntilCollision board piece ((x, y):xs)
    | isOutOfBounds (x, y) = []
    | isCollision board piece (x, y) = [(x, y)]
    | spotAvailableForPiece board piece (x, y) = [(x, y)] ++ moveUntilCollision board piece xs
    | otherwise = []

rookMoves :: Board -> (Int, Int) -> [(Int, Int)]
rookMoves board (x, y) =
    let piece = getPiece board (x, y) in
    concatMap (moveUntilCollision board piece)
        [
            [(x + n, y) | n <- [1..8]],
            [(x - n, y) | n <- [1..8]],
            [(x, y + n) | n <- [1..8]],
            [(x, y - n) | n <- [1..8]]
        ]

knightMoves :: Board -> (Int, Int) -> [(Int, Int)]
knightMoves board (x, y) =
    let piece = getPiece board (x, y) in
    filter (spotAvailableForPiece board piece)
        [(x + 2, y - 1), (x + 2, y + 1), (x - 2, y - 1), (x - 2, y + 1),
         (x + 1, y - 2), (x + 1, y + 2), (x - 1, y - 2), (x - 1, y + 2)]

bishopMoves :: Board -> (Int, Int) -> [(Int, Int)]
bishopMoves board (x, y) =
    let piece = getPiece board (x, y) in
    concatMap (moveUntilCollision board piece)
        [
            [(x + n, y + n) | n <- [1..8]],
            [(x + n, y - n) | n <- [1..8]],
            [(x - n, y + n) | n <- [1..8]],
            [(x - n, y - n) | n <- [1..8]]
        ]

kingMoves :: Board -> (Int, Int) -> [(Int, Int)]
kingMoves board (x, y) =
    let piece = getPiece board (x, y) in
        filter (spotAvailableForPiece board piece)
        [
          (x + 1, y),
          (x - 1, y),
          (x, y + 1),
          (x, y - 1),
          (x + 1, y + 1),
          (x + 1, y - 1),
          (x - 1, y + 1),
          (x - 1, y - 1)
        ]

queenMoves :: Board -> (Int, Int) -> [(Int, Int)]
queenMoves board (x, y) =
    rookMoves board (x, y) ++ bishopMoves board (x, y)

canAttack :: Board -> Color -> (Int, Int) -> Bool
canAttack board color (x, y) =
    let piece = getPiece board (x, y)
        otherColor = getColor piece in
        otherColor /= None && color /= None && color /= otherColor

pawnAttackMoves :: Board -> (Int, Int) -> [(Int, Int)]
pawnAttackMoves board (x, y) =
    let color = getColor $ getPiece board (x, y)
        whiteAttacks = [(x + 1, y - 1), (x - 1, y - 1)]
        blackAttacks = [(x + 1, y + 1), (x - y, y + 1)] in
        if color == White then
            filter (canAttack board color) whiteAttacks
        else
            filter (canAttack board color) blackAttacks

pawnDoubleCollision :: Board -> (Int, Int) -> Bool
pawnDoubleCollision board (x, y) =
    let color = getColor $ getPiece board (x, y) in
    case color of
        White -> (getPiece board (x, y - 1) /= Empty) || (getPiece board (x, y - 2) /= Empty)
        Black -> (getPiece board (x, y + 1) /= Empty) || (getPiece board (x, y + 2) /= Empty)
        None  ->  True

pawnDoubleMove :: Board -> (Int, Int) -> [(Int, Int)]
pawnDoubleMove board (x, y) =
    let color = getColor $ getPiece board (x, y) in
    if color == White && y == 6 && not (pawnDoubleCollision board (x, y)) then
        [(x, y - 2)]
    else if color == Black && y == 1 && not (pawnDoubleCollision board (x, y)) then
        [(x, y + 2)]
    else
        []

pawnMoves :: Board -> (Int, Int) -> [(Int, Int)]
pawnMoves board (x, y) =
    let piece = getPiece board (x, y)
        color = getColor piece in
    case color of
        White -> pawnAttackMoves board (x, y) ++
                 pawnDoubleMove board (x, y) ++
                 if getPiece board (x, y - 1) == Empty then [(x, y - 1)] else []

        Black -> pawnAttackMoves board (x, y) ++
                 pawnDoubleMove board (x, y) ++
                 if getPiece board (x, y + 1) == Empty then [(x, y + 1)] else []
        None  -> []

movesForPiece :: Board -> (Int, Int) -> [(Int, Int)]
movesForPiece board (x, y) =
    let piece = getPiece board (x, y) in
    case piece of
        Piece { pName = Queen }  -> queenMoves board (x, y)
        Piece { pName = Bishop } -> bishopMoves board (x, y)
        Piece { pName = Knight } -> knightMoves board (x, y)
        Piece { pName = King }   -> kingMoves board (x, y)
        Piece { pName = Rook }   -> rookMoves board (x, y)
        Piece { pName = Pawn }   -> pawnMoves board (x, y)
        _ -> []
