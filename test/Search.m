MODULE Search;

IMPORT Plane, Display, Terminal, Bit, Strings;

PROCEDURE Search(p: INTEGER; patt: ARRAY OF CHAR): INTEGER;
  VAR s, i: INTEGER;
BEGIN
  s := p;
  WHILE Plane.buf.Peek(s) # 0X DO
    i := 0;
    WHILE (patt[i] # 0X) & (patt[i] = Plane.buf.Peek(s+i)) DO i := i+1 END;
    IF patt[i] = 0X THEN RETURN s+i END;
    s := s+1
  END;
  RETURN -1
END Search;

PROCEDURE Find(p: INTEGER; patt: ARRAY OF CHAR): BOOLEAN;
  VAR t: INTEGER;
BEGIN
  t := Search(p, patt);
  IF t >= 0 THEN Plane.SetPosition(t); RETURN TRUE END;
  t := Search(0, patt);
  IF t >= 0 THEN Plane.SetPosition(t); RETURN TRUE END;
  Terminal.Ding; RETURN FALSE
END Find;

CONST 
  sChar = 1; sDel = 2; sSearch = 3; sQuit = 4; sDone = 5; sAbort = 6;

VAR 
  searchmap: ARRAY Terminal.NKEYS OF INTEGER;
  lastsearch: ARRAY 80 OF CHAR;

PROCEDURE Ctrl(c: CHAR): INTEGER;
BEGIN
  RETURN Bit.Xor(ORD(c), 40H)
END Ctrl;

PROCEDURE InitSearch;
  VAR i: INTEGER;
BEGIN
  FOR i := 0 TO Terminal.NKEYS-1 DO searchmap[i] := sDone END;
  FOR i := 20H TO 7EH DO searchmap[i] := sChar END;
  searchmap[Ctrl('S')] := sSearch;
  searchmap[Ctrl('J')] := sQuit;
  searchmap[Ctrl('M')] := sQuit;
  searchmap[Ctrl('?')] := sDel;
  searchmap[Ctrl('H')] := sDel;
  searchmap[Ctrl('G')] := sAbort;

  COPY("", lastsearch)
END InitSearch;

(* |ISearch| -- incremental search a la Emacs *)
PROCEDURE ISearch*;
  CONST prompt = "I-Search:";
  VAR 
    k: Terminal.key;
    n, r: INTEGER;
    buf: ARRAY 80 OF CHAR;
    cmd, pos: ARRAY 100 OF INTEGER;
    dummy: BOOLEAN;
BEGIN
  Display.ShowMinibuf(prompt, "");
  n := 0; r := 0; buf[0] := 0X;
  LOOP
    Terminal.GetKey(k);
    CASE searchmap[k] OF
      sChar:
	buf[n] := CHR(k); buf[n+1] := 0X; n := n+1; 
	cmd[r] := sChar; pos[r] := Plane.buf.pos; r := r+1;
	dummy := Find(Plane.buf.pos-(n-1), buf);
    | sSearch:
	cmd[r] := sSearch; pos[r] := Plane.buf.pos;
	IF n = 0 THEN 
	  COPY(lastsearch, buf); n := Strings.Length(buf);
	  dummy := Find(Plane.buf.pos, buf); r := r+1
        ELSE
  	  IF Find(Plane.buf.pos, buf) THEN r := r+1 END
	END;
    | sDel:
	IF r > 0 THEN 
	  r := r-1; Plane.SetPosition(pos[r]);
	  CASE cmd[r] OF
	    sChar:
	      n := n-1; buf[n] := 0X 
	  | sSearch:
	      IF r = 0 THEN n := 0; buf[0] := 0X END
	  END;
        END
    | sQuit:
	COPY(buf, lastsearch);
	EXIT
    | sDone:
	COPY(buf, lastsearch);
	Terminal.Pushback(k); EXIT
    | sAbort:
	Terminal.Ding;
	IF r > 0 THEN Plane.SetPosition(pos[0]) END;
        EXIT
    END;
    Display.CheckMovement;
    Display.ShowMinibuf(prompt, buf)
  END;
  Display.CheckMovement;
  Display.CloseMinibuf
END ISearch;

BEGIN
  InitSearch
END Search.
