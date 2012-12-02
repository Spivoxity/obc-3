MODULE Plane;

(* pp/ewoks/Plane.m *)

(* This module augments |Buffer| with routines that maintain a 
   two-dimen\-sional view of the text as a sequence of lines. 
   Most of the operations provided are extensions of the
   operations provided by |Buffer| that also update the current
   coordinates in the plane. *)

IMPORT Buffer;

CONST EOL* = 0AX; NUL* = Buffer.NUL;   

CONST MAX = 10000; (* Max no.\ of lines *)

VAR 
  x-, y-, ymax-: INTEGER;   
  x0: INTEGER; (* Start of current line *)
  len: ARRAY MAX OF INTEGER; (* Length of each line *)
  buf*: Buffer.Buffer; (* The buffer *)

PROCEDURE Sanity;
BEGIN
  ASSERT(x >= 0);
  IF y > 0 THEN ASSERT(buf.Peek(x0-1) = EOL) END;
  ASSERT(buf.pos = x0+x)
END Sanity;

(* |MapLines| -- recalculate |len| array and |ymax| *)
PROCEDURE MapLines;
  VAR i, k, beg: INTEGER;
BEGIN
  (* Map the lines *)
  beg := 0; k := 0; i := 0;
  WHILE i < buf.len DO  
    IF buf.Peek(i) = EOL THEN
      len[k] := i - beg + 1;
      k := k+1; beg := i+1
    END;
    i := i+1
  END;
  len[k] := i - beg;
  ymax := k;
END MapLines;

PROCEDURE Init*;
BEGIN
  buf.SetPosition(0);
  x := 0; x0 := 0; y := 0;
  MapLines
END Init;   

PROCEDURE MoveLeft*;
BEGIN
  buf.MoveLeft;
  IF x > 0 THEN
    x := x - 1
  ELSIF y > 0 THEN
    y := y-1; x0 := x0 - len[y]; 
    x := len[y] - 1;
  END
; Sanity
END MoveLeft;

PROCEDURE MoveRight*;
  VAR ch: CHAR;
BEGIN
  ch := buf.Peek(buf.pos);
  IF ch # NUL THEN
    buf.MoveRight;
    IF ch # EOL THEN
      x := x+1
    ELSE
      x0 := x0 + len[y]; y := y+1; x := 0
    END
  END
; Sanity
END MoveRight;

PROCEDURE InsertLeft*(ch: CHAR);
  VAR i: INTEGER;
BEGIN
  buf.InsertLeft(ch);
  x := x+1; len[y] := len[y]+1;
  IF ch = EOL THEN
    (* Split the line *)
    FOR i := ymax TO y+1 BY -1 DO len[i+1] := len[i] END;
    len[y+1] := len[y] - x;
    len[y] := x;
    ymax := ymax+1; x0 := x0 + len[y]; y := y+1; x := 0
  END
; Sanity
END InsertLeft;

PROCEDURE InsertRight*(ch: CHAR);
BEGIN
  InsertLeft(ch); MoveLeft
END InsertRight;

PROCEDURE DeleteLeft*;
  VAR i: INTEGER;
BEGIN
  IF (x = 0) & (y > 0) THEN
    (* Join two lines *)
    y := y-1; x0 := x0 - len[y]; ymax := ymax - 1;
    x := len[y]; len[y] := len[y] + len[y+1];
    FOR i := y+1 TO ymax DO len[i] := len[i+1] END
  END;

  IF x > 0 THEN
    buf.DeleteLeft; 
    x := x-1; len[y] := len[y]-1
  END
; Sanity
END DeleteLeft;

PROCEDURE DeleteRight*;
BEGIN
  IF buf.Peek(buf.pos) # NUL THEN
    MoveRight; DeleteLeft
  END
END DeleteRight;

(* |Goto| -- go to specified |(x, y)| coords, or as close as possible *)
PROCEDURE Goto*(x1, y1: INTEGER);
BEGIN
  IF y >= y1 THEN
    WHILE (y > y1) & (y > 0) DO y := y-1; x0 := x0 - len[y] END
  ELSE
    WHILE (y < y1) & (y < ymax) DO x0 := x0 + len[y]; y := y+1 END
  END;
  
  (* If actually on line |y1|, move to column |x1| *)
  IF y > y1 THEN
    x := 0
  ELSIF (y = ymax) & ((y < y1) OR (x1 >= len[y])) THEN
    x := len[y]
  ELSIF x1 >= len[y] THEN
    x := len[y]-1
  ELSE
    x := x1
  END;

  buf.SetPosition(x0+x)
; Sanity
END Goto;
    
PROCEDURE SetPosition*(p: INTEGER);
BEGIN
  buf.SetPosition(p);
  y := 0; x0 := 0;
  WHILE x0 + len[y] < p DO x0 := x0 + len[y]; y := y+1 END;
  x := p - x0
; Sanity
END SetPosition;

BEGIN
  buf.Clear
END Plane.
