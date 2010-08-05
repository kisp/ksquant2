all:
	ghc -fglasgow-exts --make Main.hs
	./Main


test:
	runghc HelloTest.hs 2>/dev/null | tee test.log 
	tail -n 1 <test.log | grep 'tests passed'
	rm -f test.log

test2:
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
