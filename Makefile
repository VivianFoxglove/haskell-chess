SRC=Board.hs Piece.hs Main.hs
TARGET=./Main

all:
	ghc --make ${SRC}

run: all
	${TARGET}

clean:
	rm -f *.{o,hi} ${TARGET}
