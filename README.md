[![Build Status](https://travis-ci.org/kisp/ksquant2.svg?branch=master)](https://travis-ci.org/kisp/ksquant2)


Compilation of ksquant2 kernel
===============================

The ksquant2 kernel is written in Haskell, so you need a working Haskell installation.

The following commands set up a cabal sandbox, so Haskell dependencies that are needed for
compilation of ksquant2 are only installed locally inside the ksquant2 directory.

```
$ cd /path/to/ksquant2
$ cabal sandbox init                   # Initialise the sandbox
$ cabal install --only-dependencies    # Install dependencies into the sandbox
$ cabal build                          # Build ksquant2
```

Installation
=====================

Then it can be installed as a normal PWGL user-library in
$HOME/PWGL-User/User-library or the corresponding place on
windows. Please make sure that after downloading and extracting the
archive the folder is named "ksquant2".

Bugs
=================

Bugs can be browsed and reported here:
https://github.com/kisp/ksquant2/issues


Changes
====================

* changes in 0.1.10 relative to 0.1.9:
  * bugfix: for voices containing only a rest
  * new feature: support for windows

* changes in 0.1.9 relative to 0.1.8:
  * bugfix: the correct way to indicate a rest is e.g. (6.875 :REST T)
            the keyword is :REST, not :RESTP
  * bugfix: It was not possible to start with a rest, i.e. (0.0 :REST T)
            did not work correctly


License
================================

This file is part of KSQuant2.

Copyright (c) 2010, Kilian Sprotte. All rights reserved.

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.
