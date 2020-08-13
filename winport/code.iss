; 
; code.iss
; 
; This file is part of the Oxford Oberon-2 compiler
; Copyright (c) 2006--2016 J. M. Spivey
; All rights reserved
;
; Redistribution and use in source and binary forms, with or without
; modification, are permitted provided that the following conditions are met:
;
; 1. Redistributions of source code must retain the above copyright notice,
;    this list of conditions and the following disclaimer.
; 2. Redistributions in binary form must reproduce the above copyright notice,
;    this list of conditions and the following disclaimer in the documentation
;    and/or other materials provided with the distribution.
; 3. The name of the author may not be used to endorse or promote products
;    derived from this software without specific prior written permission.
;
; THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
; IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
; OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
; IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
; SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
; PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS;
; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
; OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
; ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;

[Code]
{ Under WinNT, add the bin subdirectory to the system path }

{ SplitPath -- split a path into individual directories }
procedure SplitPath(path: string; var dirs: TStringList);
  var i: integer; s: string;
begin
  dirs := TStringList.Create;
  s := ''; 

  for i := 1 to Length(path) do begin
    if path[i] <> ';' then s := s + path[i];

    if (path[i] = ';') or (i = Length(path)) then begin
      s := Trim(RemoveQuotes(Trim(s)));
      if s <> '' then dirs.Add(s);
      s := ''
    end
  end
end;

{ ConcatPath -- concatenate individual directories into a path }
procedure ConcatPath(dirs: TStringList; var path: string);
  var i: integer;
begin
  path := '';
  for i := 0 to dirs.Count - 1 do begin
    if i > 0 then path := path + ';'
    path := path + dirs.Strings[i]
  end
end;

{ AddToPathString -- add directory to a path string }
procedure AddToPathString(dir: string; var path: string);
var
  dirs: TStringList;
  absent: Boolean;
  i: integer;
begin
  dir := Trim(RemoveQuotes(Trim(dir)));
  SplitPath(path, dirs);

  // Check if dir is already in path
  absent := True;
  for i := 1 to dirs.Count - 1 do begin
    if uppercase(dirs.Strings[i]) = uppercase(dir) then 
      absent := False
  end;

  if absent then dirs.Append(dir);
  ConcatPath(dirs, path);
  dirs.Free
end;

{ AddToPath -- add a directory to the path }
procedure AddToPath(dir: string);
  var
    rootkey: integer;
    subkey: string;
    name: string;
    path: string;
begin
  if not UsingWinNt then Exit;

  // Perform directory constant expansion
  dir := ExpandConstantEx(dir, ' ', ' ');

  rootkey := HKEY_LOCAL_MACHINE;
  subkey := 'SYSTEM\CurrentControlSet\Control\Session Manager\Environment';
  name := 'Path';

  if not RegQueryStringValue(rootkey, subkey, name, path) then Exit;
  AddToPathString(dir, path);
  if not RegWriteStringValue(rootkey, subkey, name, path) then Exit
end;

{ CurStepChanged -- event procedure called for each step of installation }
procedure CurStepChanged(step: TSetupStep);
begin
  if step = ssDone then AddToPath('{app}\\bin')
end;
