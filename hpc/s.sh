#!/bin/bash

set -e

git clean -fxd

cabal configure --enable-tests

cabal build

cabal test

clisp -norc lisp/test-runner.lisp

rm -f tmp
mkdir bin tmp
ln -s ../dist/build/ksquant2/ksquant2 bin/ksquant2

cucumber

# ./bin/ksquant2 --version || true

find -name \*.tix
hpc combine --union ksquant2.tix tests.tix >comb.tix

# hpc combine --union tmp/aruba/ksquant2.tix comb.tix >comb2.tix

hpc report comb.tix

hpc markup --destdir=/tmp/cov comb.tix >/dev/null
