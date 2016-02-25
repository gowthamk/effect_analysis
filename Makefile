all:
	ghc -o parser parser.hs

clean:
	rm *.hi *.o parser
