all:
	ghc -o parser rfParser.hs

clean:
	rm *.hi *.o parser
