MODULE Keymap;

(* pp/ewoks/Keymap.m *)

(* This module maintains a map that binds keys to editor commands. 
   A command is simply a procedure with no parameters or result, and
   the keymap is an array of procedure values.  Although the editor
   provides no way to do it at present, this means that key bindings
   could be changed in response to commands. *)

IMPORT Terminal, Editor, Search, Bit;

VAR map: ARRAY Terminal.NKEYS OF Editor.command;

(* |DoCommand| -- carry out a command *)
PROCEDURE DoCommand*(cmd: Terminal.key);
BEGIN
  Editor.DoCommand(map[cmd], cmd)
END DoCommand;

PROCEDURE Ctrl(c: CHAR): INTEGER;
BEGIN
  RETURN Bit.Xor(ORD(c), 40H)
END Ctrl;

PROCEDURE Init;
  VAR i: INTEGER;
BEGIN
  FOR i := 0 TO Terminal.NKEYS-1 DO map[i] := Terminal.Ding END;

  (* All printing characters insert themselves *)
  FOR i := 20H TO 7EH DO map[i] := Editor.SelfInsert END;

  (* Emacs-like bindings for control characters *)
  map[Ctrl('A')] := Editor.CursorHome;
  map[Ctrl('B')] := Editor.MoveLeft;
  map[Ctrl('D')] := Editor.DeleteRight;
  map[Ctrl('E')] := Editor.CursorEnd;
  map[Ctrl('F')] := Editor.MoveRight;
  map[Ctrl('H')] := Editor.DeleteLeft;
  map[Ctrl('J')] := Editor.InsertLine;
  map[Ctrl('K')] := Editor.DeleteLine;
  map[Ctrl('L')] := Editor.Redraw;
  map[Ctrl('M')] := Editor.InsertLine;
  map[Ctrl('N')] := Editor.MoveDown;
  map[Ctrl('P')] := Editor.MoveUp;
  map[Ctrl('Q')] := Editor.Quit;
  map[Ctrl('S')] := Search.ISearch;
  map[Ctrl('?')] := Editor.DeleteLeft;

  (* Bindings for arrow keys, etc. *)
  map[Terminal.Up] := Editor.MoveUp;
  map[Terminal.Down] := Editor.MoveDown;
  map[Terminal.Right] := Editor.MoveRight;
  map[Terminal.Left] := Editor.MoveLeft;
  map[Terminal.Home] := Editor.CursorHome;
  map[Terminal.End] := Editor.CursorEnd;
  map[Terminal.PageUp] := Editor.PageUp;
  map[Terminal.PageDown] := Editor.PageDown
END Init;

BEGIN
  Init
END Keymap.
