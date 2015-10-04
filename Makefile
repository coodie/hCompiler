all: main

main:
	ghc  src/Main.hs -isrc --make -o hCompiler -outputdir build

clean:
	rm -R build
