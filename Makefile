all:
	ghc --make Main.hs
	./Main

test:
	runhaskell -XMultiParamTypeClasses -XFunctionalDependencies -XFlexibleInstances IntervalTest.hs 

clean:
	rm -f *flymake* *.hi *.o Main
