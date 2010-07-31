all:
	ghc --make Main.hs

clean:
	rm -f *flymake* *.hi *.o Main
