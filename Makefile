CC     = ghc
CFLAGS = -main-is TicTacToe.main --make
BUILD  = TicTacToe

all:	$(BUILD)

clean:
	rm -f TicTacToe *.hi *.o

TicTacToe:	TicTacToe.hs
	$(CC) $(CFLAGS) TicTacToe.hs
