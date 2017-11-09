import Board
import Piece

main :: IO ()
main = do
    board <- pure newBoard
    board <- movePiece board (4, 1) (4, 2)
    -- board <- pure $ setPiece board (0, 0) (Piece { pName = Queen, pColor = White })
    -- print $ getPiece board (4, 0)
    printBoard board
