import Board

main :: IO ()
main = do
    board <- pure newBoard
    print $ getPiece board E1
    printBoard board
