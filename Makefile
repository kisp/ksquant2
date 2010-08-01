all:
	ghc --make Main.hs
	./Main

clean:
	rm -f *flymake* *.hi *.o Main
