all:
	ghc -fglasgow-exts --make Main.hs
	./Main

test:
	runhaskell -XMultiParamTypeClasses -XFunctionalDependencies -XFlexibleInstances IntervalTest.hs 

clean:
	git clean -f -x -d 

# ----------------------------------------------------------------

configure: 
	runghc Setup.hs configure

doc: configure
	runghc Setup.hs haddock --executables

build: configure
	runghc Setup.hs build
