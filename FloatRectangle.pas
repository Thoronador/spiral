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

unit FloatRectangle;

interface

uses
  Classes, Direction, FloatPoint, FloatSquare, GL;

type
  { class to represent a rectangle in 2D space with floating point coordinates } 
  TFloatRectangle = class
    private
      m_BottomLeft: TFloatPoint;
      m_Width:      Single;
      m_Height:     Single;
    public
      constructor Create(const bottomLeft: TFloatPoint; const width, height: Single);
      function BottomLeft: TFloatPoint;
      function TopRight:   TFloatPoint;
      function Expand(const square: TFloatSquare; const d: TDirection): Boolean;
      procedure Draw;
  end; //class TFloatRectangle

implementation

constructor TFloatRectangle.Create(const bottomLeft: TFloatPoint; const width, height: Single);
begin
  m_BottomLeft := bottomLeft;
  m_Width := width;
  if (m_Width < 0.0) then
  begin
    //replace unreasonable value with length zero
    //Note: We deliberately allow degradation to a point here, i.e. setting
    //both width and height to zero.
    m_Width := 0.0;
  end;
  m_Height := height;
  if (m_Height < 0.0) then
  begin
    //replace unreasonable value with length zero
    m_Height := 0.0;
  end;
end; //constructor

function TFloatRectangle.BottomLeft: TFloatPoint;
begin
  Result := m_BottomLeft;
end;

function TFloatRectangle.TopRight: TFloatPoint;
begin
  Result := TFloatPoint.Create(m_BottomLeft.X + m_Width, m_BottomLeft.Y + m_Height);
end;

function TFloatRectangle.Expand(const square: TFloatSquare; const d: TDirection): Boolean;
begin
  case d of
    dirWest, dirEast: if (m_Height <> square.Length) then Exit(false);
    dirNorth, dirSouth: if (m_Width <> square.Length) then Exit(false);
  end; //case

  Result := true;
  case d of
    dirWest: begin
               m_Width := m_Width + square.Length;
               m_BottomLeft := TFloatPoint.Create(m_BottomLeft.X - square.Length, m_BottomLeft.Y);
             end;
    dirEast: m_Width := m_Width + square.Length;
    dirNorth: m_Height := m_Height + square.Length;
    dirSouth: begin
                m_Height := m_Height + square.Length;
                m_BottomLeft := TFloatPoint.Create(m_BottomLeft.X, m_BottomLeft.Y - square.Length);
              end;
  else
    Result := false; //will never happen
  end; //case
end; //func

procedure TFloatRectangle.Draw;
begin
  glBegin(GL_LINE_LOOP);
    glVertex2f(m_BottomLeft.X, m_BottomLeft.Y);
    glVertex2f(m_BottomLeft.X + m_Width, m_BottomLeft.Y);
    glVertex2f(m_BottomLeft.X + m_Width, m_BottomLeft.Y - m_Height);
    glVertex2f(m_BottomLeft.X, m_BottomLeft.Y - m_Height);
  glEnd;
end; //proc

end.
