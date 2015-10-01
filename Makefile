all: main

main:
	ghc  Main.hs --make -o Main -outputdir build

clean:
	rm *.o *.hi
