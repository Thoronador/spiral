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

unit FloatSquare;

interface

uses
  Classes, FloatPoint;

type
  { class to represent a square in 2D space with floating point coordinates } 
  TFloatSquare = class
    private
      m_BottomLeft: TFloatPoint;
      m_Length:      Single;
    public
      constructor Create(const bottomLeft: TFloatPoint; width: Single);

      function BottomLeft: TFloatPoint;
      function TopRight:   TFloatPoint;
      function Length:     Single;
  end; //class TFloatSquare

implementation

constructor TFloatSquare.Create(const bottomLeft: TFloatPoint; width: Single);
begin
  m_BottomLeft := bottomLeft;
  m_Length := width;
  if (m_Length <= 0.0) then
  begin
    //replace unreasonable value with length one
    m_Length := 1.0;
  end;
end;

function TFloatSquare.BottomLeft: TFloatPoint;
begin
  Result := m_BottomLeft;
end;

function TFloatSquare.TopRight: TFloatPoint;
begin
  Result := TFloatPoint.Create(m_BottomLeft.X + m_Length, m_BottomLeft.Y + m_Length);
end;

function TFloatSquare.Length: Single;
begin
  Result := m_Length;
end;

end.
