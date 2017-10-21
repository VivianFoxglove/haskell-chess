SRC=Board.hs Piece.hs Main.hs
TARGET=./Main

all:
	cd src
	ghc --make ${SRC}

run: all
	${TARGET}

clean:
	rm -f *.{o,hi} ${TARGET}
