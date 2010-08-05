all:
	ghc -fglasgow-exts --make Main.hs
	./Main

.PHONY: test
test:
	ghc -fglasgow-exts -fhpc --make Test.hs -o test
	./test 2>/dev/null >test.log
	tail -n 1 <test.log | grep 'tests passed'
	cat test.log
	rm -f test.log
	hpc markup --destdir=test-coverage test 2>/dev/null >/dev/null
	hpc report test

clean:
	git clean -f -x -d 

# ----------------------------------------------------------------

configure: 
	runghc Setup.hs configure

doc: configure
	runghc Setup.hs haddock --executables

build: configure
	runghc Setup.hs build
