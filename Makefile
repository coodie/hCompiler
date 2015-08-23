all: main

main:
	ghc  Main.hs --make -o Main

clean:
	rm *.o *.hi
