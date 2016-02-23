BIN=bin/problem535
FLAGS=-O2 -Wall -fwarn-tabs --make -fforce-recomp -o $(BIN)

all: test

test: $(BIN)
	time $(BIN)

$(BIN): problem535.hs
	mkdir -p bin
	ghc $(FLAGS) problem535.hs

hlint:
	hlint .

lint: hlint

clean:
	-rm -rf bin
	-rm *.o
	-rm *.hi
