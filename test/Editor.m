MODULE Editor;

(* pp/ewoks/Editor.m *)

(* The procedures exported by this module combine operations on the
   buffer with operations that keep the display up to date, and are
   suitable for binding to keys on the keyboard. 

   Commands should be invoked via the |DoCommand| procedure, which
   provides a place to put actions common to all commands. *)

IMPORT Plane, Display, Terminal;

CONST EOL = Plane.EOL; NUL = Plane.NUL;

TYPE command* = PROCEDURE;

VAR 
  active-: BOOLEAN;		(* cleared by quit command *)
  thechar: Terminal.key;	(* command character *)
  goal, prevgoal: INTEGER;	(* column goal for up/down *)

(* |Redraw| -- centre the cursor and redraw the display *)
PROCEDURE Redraw*;
BEGIN
  Display.ChooseOrigin;
  Display.Refresh
END Redraw;

(* |MoveRight| -- move one character to the right *)
PROCEDURE MoveRight*;
BEGIN
  Plane.MoveRight;
  Display.CheckMovement;
END MoveRight;

(* |MoveRight| -- move one character to the left *)
PROCEDURE MoveLeft*;
BEGIN
  Plane.MoveLeft;
  Display.CheckMovement
END MoveLeft;

(* |MoveDown| -- move up to nearest character of previous line *)
PROCEDURE MoveDown*;
BEGIN
  IF prevgoal >= 0 THEN
    goal := prevgoal
  ELSE
    goal := Plane.x
  END;
  Plane.Goto(goal, Plane.y+1);
  Display.CheckMovement
END MoveDown;

(* |MoveUp| -- move up to nearest character of previous line *)
PROCEDURE MoveUp*;
BEGIN
  IF prevgoal >= 0 THEN
    goal := prevgoal
  ELSE
    goal := Plane.x
  END;
  Plane.Goto(goal, Plane.y-1);
  Display.CheckMovement
END MoveUp;

(* |PageDown| -- move down one page *)
PROCEDURE PageDown*;
  VAR delta: INTEGER;
BEGIN
  delta := Terminal.HEIGHT - 2;
  Plane.Goto(0, Plane.y + delta);
  Display.SetOrigin(Display.origin + delta);
  Display.Refresh
END PageDown;

(* |PageUp| -- move up one page *)
PROCEDURE PageUp*;
  VAR delta: INTEGER;
BEGIN
  delta := Terminal.HEIGHT - 2;
  Plane.Goto(0, Plane.y - delta);
  Display.SetOrigin(Display.origin - delta);
  Display.Refresh
END PageUp;

(* |InsertLeft| -- insert a character to the left of the cursor *)
PROCEDURE InsertLeft(ch: CHAR);
BEGIN
  Plane.InsertLeft(ch);
  IF ch # EOL THEN 
    Display.RefreshLine
  ELSE
    Display.Refresh
  END
END InsertLeft;

(* |SelfInsert| -- ordinary key; insert the char itself *)
PROCEDURE SelfInsert*;
BEGIN
  InsertLeft(CHR(thechar))
END SelfInsert;

(* |InsertLine| -- insert a newline *)
PROCEDURE InsertLine*;
BEGIN
  InsertLeft(EOL)
END InsertLine;

(* |CursorHome| -- Move to beginning of line *)
PROCEDURE CursorHome*;
BEGIN
  Plane.Goto(0, Plane.y);
  Display.CheckMovement
END CursorHome;

(* |CursorEnd| -- Move to end of line *)
PROCEDURE CursorEnd*;
  VAR ch: CHAR;
BEGIN
  LOOP
    ch := Plane.buf.Peek(Plane.buf.pos);
    IF (ch = EOL) OR (ch = NUL) THEN EXIT END;
    Plane.MoveRight
  END;
  Display.CheckMovement
END CursorEnd;

(* |DeleteRight| -- delete a character to the right of the cursor *)
PROCEDURE DeleteRight*;
  VAR ch: CHAR;
BEGIN
  ch := Plane.buf.Peek(Plane.buf.pos);
  Plane.DeleteRight;
  IF ch # EOL THEN
    Display.RefreshLine
  ELSE
    Display.Refresh
  END
END DeleteRight;

(* |DeleteLeft| -- delete a character to the left of the cursor *)
PROCEDURE DeleteLeft*;
  VAR ch: CHAR;
BEGIN
  ch := Plane.buf.Peek(Plane.buf.pos-1);
  Plane.DeleteLeft;
  IF ch # EOL THEN
    Display.RefreshLine
  ELSE
    Display.Refresh
  END
END DeleteLeft;

(* |DeleteLine| -- delete to end of line, or join lines if at end *)
PROCEDURE DeleteLine*;
  VAR ch: CHAR;
BEGIN
  ch := Plane.buf.Peek(Plane.buf.pos);
  IF ch = EOL THEN
    Plane.DeleteRight;
    Display.Refresh
  ELSE
    REPEAT 
      Plane.DeleteRight; ch := Plane.buf.Peek(Plane.buf.pos)
    UNTIL ch = EOL;
    Display.RefreshLine
  END
END DeleteLine;      

(* |Quit| -- clear the |active| flag to quit the editor *)
PROCEDURE Quit*;
BEGIN
  active := FALSE
END Quit;

(* |DoCommand| -- common wrapper for all commands *)
PROCEDURE DoCommand*(cmd: command; k: Terminal.key);
BEGIN
  thechar := k;
  prevgoal := goal;
  goal := -1;
  cmd
END DoCommand;

BEGIN
  active := TRUE;
  goal := -1
END Editor.
