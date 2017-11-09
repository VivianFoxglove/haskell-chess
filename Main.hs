import Board
import Piece
import Moves

doMove (x1, y1) (x2, y2) = do
    board <- pure newBoard
    board <- movePiece board (x1, y1) (x2, y2)
    printBoard board
    putStrLn "-------------"

main :: IO ()
main = do
    board <- pure newBoard
    -- board <- pure $ setPiece board (1, 1) (Piece { pName = Rook, pColor = Black })
    -- board <- movePiece board (4, 1) (4, 2)

    -- piece <- pure $ getPiece board (4, 1)
    -- print $ piece
    -- print $ getColor piece

    -- print $ sameColor board (4, 2) (5, 1)
    -- print $ spotAvailable board (0, 0) (0, 9)
    -- print $ getPiece board (0, 2)

    -- print $ spotAvailableForPiece board (Piece { pName = Rook, pColor = White }) (0, 4)
    -- print $ knightMoves board (1, 0)

    mapM (doMove (0, 3)) (queenMoves board (0, 3))
    -- print $ rookMoves board (0, 2)

    -- print $ sameColor board (4, 1) (5, 1)

    -- board <- pure $ setPiece board (0, 0) (Piece { pName = Queen, pColor = White })
    -- print $ getPiece board (4, 0)
    -- printBoard board
    return ()
