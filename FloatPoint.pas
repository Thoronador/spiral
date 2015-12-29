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

unit FloatPoint;

interface

uses
  Classes;

type
  { class to represent a 2D point with floating point coordinates } 
  TFloatPoint = class
    private
      m_x, m_y: Single;
    public
      constructor Create(const initX, initY: Single);

      function X: Single;
      function Y: Single;
  end; //class FPoint

implementation

constructor TFloatPoint.Create(const initX, initY: Single);
begin
  m_x := initX;
  m_y := initY;
end;

function TFloatPoint.X: Single;
begin
  Result := m_x;
end;

function TFloatPoint.Y: Single;
begin
  Result := m_y;
end;

end.
