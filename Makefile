SRC=Board.hs Piece.hs Moves.hs Main.hs
TARGET=./chess
OBJ=obj

all:
	ghc ${SRC} -outputdir obj -o ${TARGET}

run: all
	${TARGET}

clean:
	rm -f ${OBJ}/*.{o,hi} ${TARGET}
