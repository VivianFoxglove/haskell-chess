# Haskell Chess Game

This is a simple implementation of a chess game in Haskell. After running the
program, each side (white or black) is prompted for a coordinate from which
to move one of their pieces to. The coordinates are represented by a two letter
word consisting of a letter from `a` to `h` and a number from `1` to `8`.
The letter represents the column of the board (a.k.a. the file) and the
number represents the row (a.k.a. the rank). If the move is invalid, and error
is printed and re-prompts the player for a pair of coordinates.

Below is a sample output of the game:
```
  +-------------------------------+
8 | R | N | B | Q | K | B | N | R |
  |---+---+---+---+---+---+---+---|
7 | P | P | P | P | P | P | P | P |
  |---+---+---+---+---+---+---+---|
6 |   |   |   |   |   |   |   |   |
  |---+---+---+---+---+---+---+---|
5 |   |   |   |   |   |   |   |   |
  |---+---+---+---+---+---+---+---|
4 |   |   |   |   |   |   |   |   |
  |---+---+---+---+---+---+---+---|
3 |   |   |   |   |   |   |   |   |
  |---+---+---+---+---+---+---+---|
2 | p | p | p | p | p | p | p | p |
  |---+---+---+---+---+---+---+---|
1 | r | n | b | q | k | b | n | r |
  +-------------------------------+
    A   B   C   D   E   F   G   H
White to move (FROM): e2
White to move (TO):   e4

  +-------------------------------+
8 | R | N | B | Q | K | B | N | R |
  |---+---+---+---+---+---+---+---|
7 | P | P | P | P | P | P | P | P |
  |---+---+---+---+---+---+---+---|
6 |   |   |   |   |   |   |   |   |
  |---+---+---+---+---+---+---+---|
5 |   |   |   |   |   |   |   |   |
  |---+---+---+---+---+---+---+---|
4 |   |   |   |   | p |   |   |   |
  |---+---+---+---+---+---+---+---|
3 |   |   |   |   |   |   |   |   |
  |---+---+---+---+---+---+---+---|
2 | p | p | p | p |   | p | p | p |
  |---+---+---+---+---+---+---+---|
1 | r | n | b | q | k | b | n | r |
  +-------------------------------+
    A   B   C   D   E   F   G   H
Black to move (FROM): a8
Black to move (TO):   a6

ERROR: Invalid move
  +-------------------------------+
8 | R | N | B | Q | K | B | N | R |
  |---+---+---+---+---+---+---+---|
7 | P | P | P | P | P | P | P | P |
  |---+---+---+---+---+---+---+---|
6 |   |   |   |   |   |   |   |   |
  |---+---+---+---+---+---+---+---|
5 |   |   |   |   |   |   |   |   |
  |---+---+---+---+---+---+---+---|
4 |   |   |   |   | p |   |   |   |
  |---+---+---+---+---+---+---+---|
3 |   |   |   |   |   |   |   |   |
  |---+---+---+---+---+---+---+---|
2 | p | p | p | p |   | p | p | p |
  |---+---+---+---+---+---+---+---|
1 | r | n | b | q | k | b | n | r |
  +-------------------------------+
    A   B   C   D   E   F   G   H
```
