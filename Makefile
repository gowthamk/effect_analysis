all:
	ghc -o parser Main.hs

clean:
	rm *.hi *.o Main
