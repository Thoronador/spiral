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
  Classes, Direction, Fibonacci, FloatPoint, FloatRectangle, FloatSquare, GL;

type
  TSpiralDrawer = class
    private
      //nothing yet
    public
      { constructor }
      constructor Create;

      { draws the spiral

        parameters:
            n - number of iterations that will be drawn
      }
      procedure Draw(n: Word);

      { draws an arc

        parameters:
            center - center of the arc
            radius - radius of the arc
            degStart - angle (in degrees) where arc starts
            degEnd   - angle (in degrees) where arc ends
            steps    - steps between begin and end
      }
      procedure DrawArc(const center: TFloatPoint; const radius, degStart, degEnd: Single; steps: Word);
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
    sq_size: LongWord;
    i: Word;
begin
  if (n > 20) then
    n := 20;

  BottomLeft := TFloatPoint.Create(0.0, 0.0);
  Rect := TFloatRectangle.Create(BottomLeft, 1, 1);
  Rect.Draw;
  currentDirection := dirWest;
  for i := 1 to n do
  begin
    sq_size := Fibonacci_iter(i);
    case currentDirection of
      dirWest: BottomLeft := TFloatPoint.Create(Rect.BottomLeft.X - sq_size,
                                                Rect.BottomLeft.Y);
      dirEast: BottomLeft := TFloatPoint.Create(Rect.BottomLeft.X + Rect.Width,
                                                Rect.BottomLeft.Y);
      dirNorth: BottomLeft := TFloatPoint.Create(Rect.BottomLeft.X,
                                                 Rect.BottomLeft.Y + Rect.Height);
      dirSouth: BottomLeft := TFloatPoint.Create(Rect.BottomLeft.X,
                                                 Rect.BottomLeft.Y - sq_size);
    end; //case
    Square := TFloatSquare.Create(bottomLeft, sq_size);
    if (not Rect.Expand(Square, currentDirection)) then
    begin
      WriteLn('Error: Could not expand rectangle during loop iteration ', i, '!');
      Exit;
    end; //if
    //Draw rectangle
    glColor3f(1.0, 1.0, 1.0);
    Rect.Draw;
    //Draw part of "spiral"
    glBegin(GL_LINES);
      glColor3f(1.0, 0.0, 0.0);
      case currentDirection of
        dirWest, dirEast: begin
                   glVertex2f(Square.BottomLeft.X, Square.BottomLeft.Y);
                   glVertex2f(Square.TopRight.X, Square.TopRight.Y);
                 end;
        dirSouth, dirNorth: begin
                   glVertex2f(Square.BottomLeft.X, Square.TopRight.Y);
                   glVertex2f(Square.TopRight.X, Square.BottomLeft.Y);
                  end;
      end; //case
    glEnd;
    //draw arc
    glColor3f(0.0, 1.0, 0.0);
    case currentDirection of
      dirWest: DrawArc(TFloatPoint.Create(Square.TopRight.X, Square.BottomLeft.Y),
                       Square.Length, 90, 180, Round(Square.Length*3));
      dirSouth: DrawArc(TFloatPoint.Create(Square.TopRight.X, Square.TopRight.Y),
                       Square.Length, 180, 270, Round(Square.Length*3));
      dirEast: DrawArc(TFloatPoint.Create(Square.BottomLeft.X, Square.TopRight.Y),
                       Square.Length, 270, 360, Round(Square.Length*3));
      dirNorth: DrawArc(TFloatPoint.Create(Square.BottomLeft.X, Square.BottomLeft.Y),
                       Square.Length, 0, 90, Round(Square.Length*3));
    end; //case
    //change direction for next iteration
    currentDirection := AdvanceDirection(currentDirection);
  end; //for
end;

procedure TSpiralDrawer.DrawArc(const center: TFloatPoint; const radius, degStart, degEnd: Single; steps: Word);
var i: Word;
    anglePerStep: Single;
    angleRadiant: Single;
begin
  if (steps < 1) then
    steps := 1;

  if (degStart = degEnd) or (radius <= 0.0) then
    Exit;

  anglePerStep := (degEnd - degStart) / steps;

  glBegin(GL_LINE_STRIP);
  for i := 0 to steps do
  begin
    angleRadiant := (degStart+i*anglePerStep) / 180.0 * Pi;
    glVertex2f(center.x + radius * cos(angleRadiant), center.y + radius * sin(angleRadiant));
  end; //for
  glEnd;
end; //func

end.

