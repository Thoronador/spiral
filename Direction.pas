{ ***************************************************************************

    This file is part of spiral.
    Copyright (C) 2015  Thoronador

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

  ***************************************************************************
}

unit Direction;

interface

type
  { enumeration type to identify the different directions }
  TDirection = (dirNorth, //north
                dirWest, //west
                dirSouth, //south
                dirEast); //east

  function AdvanceDirection(const d: TDirection): TDirection;

implementation

function AdvanceDirection(const d: TDirection): TDirection;
begin
  case d of
    dirNorth: Result := dirWest;
    dirWest: Result := dirSouth;
    dirSouth: Result := dirEast;
    dirEast: Result := dirNorth;
  end;//case
end;//func

end.
