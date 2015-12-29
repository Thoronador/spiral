#!/bin/bash

# This is a simple build script for spiral.
# Copyright (C) 2015  Thoronador
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.


# remove remains from previous builds
rm ./*.{o,ppu} &>/dev/null

# build project
fpc -S2 ./spiral.dpr

if [[ $? -ne 0 ]]
then
  echo "Build failed!"
  exit 1
else
  echo "Build was successful."
  exit 0
fi
