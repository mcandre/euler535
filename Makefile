BIN=bin/Problem535
FLAGS=-O2 -optc-O3 -optc-ffast-math -Wall -fwarn-tabs --make -fforce-recomp -o $(BIN)

all: test

test: $(BIN)
	time $(BIN)

$(BIN): Problem535.hs
	mkdir -p bin
	ghc $(FLAGS) Problem535.hs

hlint:
	hlint .

lint: hlint

clean:
	-rm -rf bin
	-rm *.o
	-rm *.hi
