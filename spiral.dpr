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

{$mode objfpc}
program spiral;

uses GLUT, SpiralDrawer;

const cWindowWidth: Integer = 640;
      cWindowHeight: Integer = 480;
      cPixelsPerUnit: Integer = 10;

procedure DrawWrapper; cdecl;
begin
  //TODO
end;

procedure IdleWrapper; cdecl;
begin
  DrawWrapper;
end;

begin
  WriteLn('glutInit...');
  glutInit(@argc, argv);
  WriteLn('glutInitDisplayMode...');
  glutInitDisplayMode(GLUT_RGB or GLUT_DOUBLE or GLUT_DEPTH);

  WriteLn('glutInitWindowPosition...');
  glutInitWindowPosition(0,0);
  WriteLn('glutInitWindowSize...');
  glutInitWindowSize(cWindowWidth, cWindowHeight);

  WriteLn('glutCreateWindow...');
  if (glutCreateWindow('Spiral')<=0) then
  begin
    WriteLn('ERROR: Could not create window.');
    Halt(1);
  end;
  //functions
  WriteLn('glutFuncs (callback)...');
  glutDisplayFunc(@DrawWrapper);
  { glutReshapeFunc(@ResizeWrapper);
  glutKeyboardFunc(@KeyWrapper);
  glutSpecialFunc(@SpecialWrapper);
  glutMouseFunc(@MouseWrapper);
  glutMotionFunc(@MouseMoveWrapper);
  glutPassiveMotionFunc(@MouseMoveWrapper); }
  glutIdleFunc(@IdleWrapper);

  WriteLn('Starting GUI...');
  //start GLUT's main loop
  //TODO!
end.

