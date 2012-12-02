MODULE Terminal;

(* pp/ewoks/Terminal.m *)

(* This module is responsible for terminal I/O.  It is specific to the
   VT--100 emulation provided by \pn{xterm}. *)

IMPORT In, Out;

CONST
  WIDTH* = 80;
  HEIGHT* = 24;

CONST ESC = 1BX;

TYPE key* = INTEGER;
CONST Up* = 513; Down* = 514; Right* = 515; Left* = 516; 
  Home* = 517; End* = 518; PageUp* = 519; PageDown* = 520;
  Unknown* = 521; NKEYS* = 522;

VAR pb: key;

(* |GetKey| -- get one keypress *)
PROCEDURE GetKey*(VAR k: key);
  VAR ch: CHAR; n: INTEGER;
BEGIN
  IF pb # 0 THEN k := pb; pb := 0; RETURN END;

  In.Char(ch);
  IF ch # ESC THEN
    (* An ordinary key, or maybe a control character *)
    k := ORD(ch)
  ELSE    
    In.Char(ch); 
    IF ch # '[' THEN k := Unknown; RETURN END;
    In.Char(ch);
    IF (ch >= '0') & (ch <= '9') THEN
      (* Extended key with code |"$[nnn~"| *)
      n := 0;
      REPEAT
        n := 10 * n + ORD(ch) - ORD('0');
        In.Char(ch)
      UNTIL (ch < '0') OR (ch > '9');
      IF ch # '~' THEN k := Unknown; RETURN END;
      CASE n OF
          5: k := PageUp
	| 6: k := PageDown
      ELSE
	k := Unknown
      END
    ELSE
      (* Special key with code |"$[x"| *)
      CASE ch OF
          'A': k := Up
        | 'B': k := Down
        | 'C': k := Right
        | 'D': k := Left
        | 'H': k := Home
        | 'F': k := End
      ELSE
        k := Unknown
      END
    END
  END
END GetKey;

PROCEDURE Pushback*(k: key);
BEGIN
  pb := k
END Pushback;

(* |Command| -- output command string, replacing |'$'| with |ESC|. *)
PROCEDURE Command(cmd: ARRAY OF CHAR);
  VAR i: INTEGER;
BEGIN
  i := 0;
  WHILE cmd[i] # 0X DO
    IF cmd[i] = '$' THEN
      Out.Char(ESC)
    ELSE
      Out.Char(cmd[i])
    END;
    i := i+1
  END
END Command;

(* |Clear| -- clear the screen *)
PROCEDURE Clear*;
BEGIN
  Command("$[H$[2J")
END Clear;

(* |ClearLine| -- clear to end of line *)
PROCEDURE ClearLine*;
BEGIN
  Command("$[K")
END ClearLine;

(* |Goto| -- position cursor at |(x,y)| *)
PROCEDURE Goto*(x, y: INTEGER);
BEGIN
  Command("$["); Out.Int(y, 0); Out.Char(';'); Out.Int(x, 0); Out.Char('H')
END Goto;

(* |Char| -- display a character at the cursor *)
PROCEDURE Char*(ch: CHAR);
BEGIN
  Out.Char(ch)
END Char;

(* |String| -- display a string at the cursor *)
PROCEDURE String*(s: ARRAY OF CHAR);
BEGIN
  Out.String(s)
END String;

(* |Ding| -- ring the bell *)
PROCEDURE Ding*;
BEGIN
  Out.Char(07X)
END Ding;

PROCEDURE RevVideo*(flag: BOOLEAN);
BEGIN
  IF flag THEN
    Command("$[7m")
  ELSE
    Command("$[27m")
  END
END RevVideo;

BEGIN
  pb := 0
END Terminal.
