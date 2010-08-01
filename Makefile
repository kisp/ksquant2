all:
	ghc --make Main.hs
	./Main

test:
	runhaskell -XMultiParamTypeClasses IntervalTest.hs 

clean:
	rm -f *flymake* *.hi *.o Main
