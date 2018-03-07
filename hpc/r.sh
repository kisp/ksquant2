set -xe

#cabal clean

cabal configure --enable-tests

cabal build

cabal test

clisp -norc lisp/test-runner.lisp

cucumber --no-color

