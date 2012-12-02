MODULE Display;

(* pp/ewoks/Display.m *)

(* This module is responsible for keeping the display consistent with
   the contents of the editor's buffer.  It relies on editor commands
   to pass information about how out-of-date the display may be, but
   beyond that there is no attempt to optimise updates.  Lines that
   are longer than the terminal width are not handled at all well! *)

IMPORT Plane, Terminal;

CONST EOL = Plane.EOL; NUL = Plane.NUL;

VAR 
  origin-: INTEGER;		(* |y| coord of top line *)
  scrolled: BOOLEAN;		(* whether |CheckScroll| reset origin *)
  minibuf: BOOLEAN;		(* whether bottom line in use *)
  caption, text: ARRAY 80 OF CHAR; (* Caption and text of bottom line *)

(* |SetOrigin| -- set the origin *)
PROCEDURE SetOrigin*(y: INTEGER);
  VAR page: INTEGER;
BEGIN
  page := Terminal.HEIGHT - 1;

  (* Ensure origin is within buffer, if possible by half a screen *)
  IF y > Plane.ymax - page DIV 2 THEN
    y := Plane.ymax - page DIV 2
  END;
  IF y < 0 THEN y := 0 END;

  (* Ensure cursor is on screen *)
  IF Plane.y < y THEN
    y := Plane.y
  ELSIF Plane.y > y + page - 1 THEN
    y := Plane.y - page + 1
  END;

  origin := y
END SetOrigin;

(* |ChooseOrigin| -- choose a new origin to centre the cursor *)
PROCEDURE ChooseOrigin*;
  VAR origin: INTEGER;
BEGIN
  origin := Plane.y - (Terminal.HEIGHT-2) DIV 2;
  IF origin < 0 THEN origin := 0 END;
  SetOrigin(origin)
END ChooseOrigin;

(* |CheckScroll| -- reset the origin if necessary *)
PROCEDURE CheckScroll;
BEGIN
  scrolled := FALSE;
  IF (Plane.y < origin) 
	OR (Plane.y > origin + Terminal.HEIGHT - 2) THEN
    scrolled := TRUE;
    ChooseOrigin
  END
END CheckScroll;

(* |CheckMovement| -- reset cursor and scroll after movement *)
PROCEDURE CheckMovement*;
BEGIN
  CheckScroll;
  IF scrolled THEN
    Refresh
  ELSE
    SetCursor
  END
END CheckMovement;

(* |SetCursor| -- move cursor to correct place *)
PROCEDURE SetCursor;
  VAR x: INTEGER;
BEGIN
  x := Plane.x+1;
  IF x > Terminal.WIDTH THEN x := Terminal.WIDTH END;
  Terminal.Goto(x, Plane.y-origin+1)
END SetCursor;

PROCEDURE RefreshMinibuf;
BEGIN
  Terminal.Goto(1, Terminal.HEIGHT);
  Terminal.ClearLine;
  IF minibuf THEN
    Terminal.RevVideo(TRUE);
    Terminal.String(caption);
    Terminal.RevVideo(FALSE);
    Terminal.Char(' ');
    Terminal.String(text)
  ELSE
    Terminal.RevVideo(TRUE);
    Terminal.String("--- EWOKS ---");
    Terminal.RevVideo(FALSE)
  END;
  SetCursor
END RefreshMinibuf;

(* |Refresh| -- refresh the display *)
PROCEDURE Refresh*;
  VAR y, x0, y0: INTEGER; ch: CHAR;
BEGIN
  CheckScroll;

  (* We make no attempt at optimisation ... *)
  x0 := Plane.x; y0 := Plane.y;
  Terminal.Clear;
  Plane.Goto(origin, 0);
  FOR y := 0 TO Terminal.HEIGHT-2 DO
    Terminal.Goto(1, y+1);
    Plane.Goto(0, origin+y);
    LOOP
      ch := Plane.buf.Peek(Plane.buf.pos);
      IF (ch = EOL) OR (ch = NUL) 
          OR (Plane.x >= Terminal.WIDTH) THEN EXIT END;
      Terminal.Char(ch);
      Plane.MoveRight
    END
  END;
  Plane.Goto(x0, y0);
  RefreshMinibuf
END Refresh;

(* |RefreshLine| -- refresh just the line containing the cursor *)
PROCEDURE RefreshLine*;
  VAR x0, y0: INTEGER; ch: CHAR;
BEGIN
  x0 := Plane.x; y0 := Plane.y;
  Terminal.Goto(1, y0-origin+1);
  Terminal.ClearLine;
  Plane.Goto(0, y0);
  LOOP
    ch := Plane.buf.Peek(Plane.buf.pos);
    IF (ch = EOL) OR (ch = NUL) 
       OR (Plane.x >= Terminal.WIDTH) THEN EXIT END;
    Terminal.Char(ch);
    Plane.MoveRight
  END;
  Plane.Goto(x0, y0);
  SetCursor
END RefreshLine;

(* |ShowMinibuf| -- open the minibuffer *)
PROCEDURE ShowMinibuf*(cap, txt: ARRAY OF CHAR);
BEGIN
  minibuf := TRUE;
  COPY(cap, caption);
  COPY(txt, text);
  RefreshMinibuf
END ShowMinibuf;

(* |CloseMinibuf| -- close the minibuffer *)
PROCEDURE CloseMinibuf*;
BEGIN
  minibuf := FALSE;
  RefreshMinibuf
END CloseMinibuf;  

BEGIN
  origin := 0;
  minibuf := FALSE;
END Display.
