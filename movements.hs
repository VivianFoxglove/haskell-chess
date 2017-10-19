type PiecePos = (Int,Int)

onBoard :: PiecePos -> Bool
onBoard (x, y) = x `elem` [1..8] && y `elem` [1..8]

rookMoveN :: PiecePos -> Int -> [PiecePos]
rookMoveN (x, y) n = [(x - n, y), (x + n, y), (x, y - n), (x, y + n)]

bishopMoveN :: PiecePos -> Int -> [PiecePos]
bishopMoveN (x, y) n = [(x + n, y + n), (x + n, y - n), (x - n, y + n), (x - n, y - n)]

queenMoveN :: PiecePos -> Int -> [PiecePos]
queenMoveN (x, y) n = (rookMoveN (x, y) n) ++ (bishopMoveN (x, y) n)

rookMove :: PiecePos -> [PiecePos]
rookMove (x, y) = filter onBoard (foldr (++) [] (map (rookMoveN (x, y)) [1..7]))

bishopMove :: PiecePos -> [PiecePos]
bishopMove (x, y) = filter onBoard (foldr (++) [] (map (bishopMoveN (x, y)) [1..7]))

queenMove :: PiecePos -> [PiecePos]
queenMove (x, y) = filter onBoard (foldr (++) [] (map (queenMoveN (x, y)) [1..7]))

main = do
    print $ queenMove (5, 5)
