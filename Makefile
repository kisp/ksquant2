all:
	ghc -fglasgow-exts --make Main.hs
	./Main


.PHONY: test
test:
	ghc -fglasgow-exts --make HelloTest.hs -o test
	./test 2>/dev/null >test.log
	tail -n 1 <test.log | grep 'tests passed'
	cat test.log
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
