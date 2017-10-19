type PiecePos = (Int,Int)

moveKnight :: PiecePos -> [PiecePos]
moveKnight (x,y) = filter onBoard
    [(x+2,y-1),(x+2,y+1),(x-2,y-1),(x-2,y+1)
    ,(x+1,y-2),(x+1,y+2),(x-1,y-2),(x-1,y+2)
    ]
    where onBoard (x,y) = x `elem` [1..8] && y `elem` [1..8]


moveKing :: PiecePos -> [PiecePos]
moveKing (x,y) = filter onBoard
    [(x+1,y+1),(x,y+1),(x+1,y),(x+1,y-1),
    (x-1,y+1),(x-1,y-1),(x,y-1),(x-1,y)
    ]
    where onBoard(x,y) = x `elem` [1..8] && y `elem` [1..8]
