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

unit SpiralDrawer;

interface

uses
  Classes, Direction, Fibonacci, FloatPoint, FloatRectangle, FloatSquare;

type
  TSpiralDrawer = class
    private
      //nothing yet
    public
      constructor Create;

      procedure Draw(n: Word);
  end; //class FPoint

implementation

constructor TSpiralDrawer.Create;
begin
  //empty
end;

procedure TSpiralDrawer.Draw(n: Word);
var BottomLeft: TFloatPoint;
    currentDirection: TDirection;
    Rect: TFloatRectangle;
    Square: TFloatSquare;
    i: Word;
begin
  if (n > 20) then
    n := 20;

  BottomLeft := TFloatPoint.Create(0.0, 0.0);
  Rect := TFloatRectangle.Create(BottomLeft, 1, 1);
  currentDirection := dirWest;
  for i := 1 to n do
  begin
    Square := TFloatSquare.Create(TFloatPoint.Create(0,0), Fibonacci_iter(i));
    if (not Rect.Expand(Square, currentDirection)) then
    begin
      WriteLn('Error: Could not expand rectangle during loop iteration ', i, '!');
      Exit;
    end; //if
    //Draw rectangle
    Rect.Draw;
    //change direction for next iteration
    currentDirection := AdvanceDirection(currentDirection);
  end; //for
end;

end.

