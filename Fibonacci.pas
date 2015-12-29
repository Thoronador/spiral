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

unit Fibonacci;

interface

  { calculates the n-th Fibonacci number, n>=0

    parameters:
        n - zero-based index of the Fibonacci number

    remarks:
       The zeroth and the first number are one.
  }
  function Fibonacci_iter(const n: Word): LongWord;

implementation

function Fibonacci_iter(const n: Word): LongWord;
var i: Word;
    last, last_but_one, fibo: LongWord;
begin
  if ((n = 0) or (n = 1)) then
    Result := 1
  else begin
      last := 1;
      last_but_one := 1;

      for i := 2 to n do
      begin
        // add the both most recent numbers
        fibo := last + last_but_one;
        // shift older numbers
        last_but_one := last;
        last := fibo;
      end; //for
      Result := fibo;
  end; //else
end;//func

end.
