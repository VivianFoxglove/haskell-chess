import Board
import Piece

main :: IO ()
main = do
    board <- pure newBoard
    board <- pure $ setPiece board (0, 0) (Piece { pName = Queen, pColor = White })
    -- print $ getPiece board (4, 0)
    printBoard board
