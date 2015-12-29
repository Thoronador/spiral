# spiral

spiral is (or better: will be) a small program to draw a "nice" spiral.

## Build status

[![Build Status](https://travis-ci.org/Thoronador/spiral.svg?branch=master)]
(https://travis-ci.org/Thoronador/spiral)

## Building from source

### Prerequisites

To build spiral from source you need a Pascal compiler (e.g. Free Pascal) and
the GLUT library or a compatible replacement like FreeGLUT.
It also helps to have Git, a distributed version control system, on your build
system to get the latest source code directly from the Git repository.

All of that can usually be installed be typing

    apt-get install fp-compiler git freeglut3

into a root terminal on a Debian-based system.

### Getting the source code

Get the source directly from Git by cloning the Git repository and change to
the directory after the repository is completely cloned:

    git clone https://github.com/Thoronador/spiral.git ./spiral
    cd spiral

That's it, you should now have the current source code of spiral on your
machine.

### Build process

The build process is relatively easy. Starting in the root directory of the
source, you can do the following steps:

    fpc -S2 spiral.dpr

### Test suite

There is no test suite yet, but I might add one in the future.

## Copyright and Licensing

Copyright 2015 Thoronador

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
