# This file is part of KSQuant2.

# Copyright (c) 2010 - 2011, Kilian Sprotte. All rights reserved.

# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

.PHONY: all
all:
	cabal clean
	cabal configure --enable-tests
	cabal build
	rm -f tests.tix
	dist/build/tests/tests
	hlint *.hs

.PHONY: shell-tests
shell-tests:
	ghc -fhpc --make Main.hs -o main
	rm -f main.tix
	clisp -norc lisp/test-runner.lisp

.PHONY: coverage
coverage: all shell-tests
	hpc sum --union main.tix tests.tix --output=total.tix
	hpc markup total.tix

.PHONY: clean
clean:
	git clean -f -x -d 
