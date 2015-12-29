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

uses GL, GLUT, SpiralDrawer;

const cWindowWidth: Integer = 1280;
      cWindowHeight: Integer = 720;
      cPixelsPerUnit: Integer = 5;

var iterations: Word;

procedure DrawWrapper; cdecl;
var sd: TSpiralDrawer;
    Xmax, Xmin, Ymax, Ymin: Single;
begin
  Xmax := cWindowWidth / cPixelsPerUnit / 2.0;
  XMin := - Xmax;
  Ymax := cWindowHeight / cPixelsPerUnit / 2.0;
  Ymin := - YMax;
  glLoadIdentity;
  glViewport(0,0, cWindowWidth, cWindowHeight);
  glOrtho(XMin, XMax, YMin, YMax, -1.0, 1.0);

  glClearColor(0.0, 0.0, 0.0, 0.0);//set black color as clear color
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);

  sd := TSpiralDrawer.Create;
  sd.Draw(iterations);
  sd := nil;

  glutSwapBuffers;
end;

procedure KeyWrapper(Key: Byte; x, y: LongInt); cdecl;
begin
  WriteLn('Key: ', Key);
  if ((Chr(Key) = 'Q') or (Chr(Key) = 'q') or (Key = 27)) then
  begin
    WriteLn('Termination requested.');
    Halt(0);
  end
  else if (Chr(Key) = '-') and (iterations > 1) then
  begin
    iterations := iterations -1;
    WriteLn('Iterations: ', iterations);
  end
  else if (Chr(Key) = '+') and (iterations < 20) then
  begin
    iterations := iterations + 1;
    WriteLn('Iterations: ', iterations);
  end;
end;

procedure IdleWrapper; cdecl;
begin
  DrawWrapper;
end;

begin
  iterations := 5;

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
  glutKeyboardFunc(@KeyWrapper);
  { glutReshapeFunc(@ResizeWrapper);
  glutSpecialFunc(@SpecialWrapper);
  glutMouseFunc(@MouseWrapper);
  glutMotionFunc(@MouseMoveWrapper);
  glutPassiveMotionFunc(@MouseMoveWrapper); }
  glutIdleFunc(@IdleWrapper);

  // Enable backface culling
  glEnable(GL_CULL_FACE);
  // Set up depth buffer
  //glEnable(GL_DEPTH_TEST);
  //glDepthFunc(GL_LESS);
  //glAlphaFunc(GL_GREATER, 0.2);
  //Starting
  WriteLn('glutMainLoop');
  glutMainLoop;
end.

