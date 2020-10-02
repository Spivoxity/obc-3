MODULE tWild;

IMPORT Out;

PROCEDURE SegLen(VAR patt: ARRAY OF CHAR; i: INTEGER): INTEGER;
  VAR k: INTEGER;
BEGIN
  k := 0;
  WHILE (patt[i+k] # 0X) & (patt[i+k] # '*') DO k := k+1 END;
  RETURN k
END SegLen;

PROCEDURE MatchSeg(VAR patt: ARRAY OF CHAR; i: INTEGER;
    VAR text: ARRAY OF CHAR; j: INTEGER): BOOLEAN;
BEGIN
  LOOP
    CASE patt[i] OF
      '*': RETURN TRUE;
    | '?': IF patt[i] = 0X THEN RETURN FALSE END
    | 0X: RETURN (text[j] = 0X)
    ELSE
      IF patt[i] # text[j] THEN RETURN FALSE END
    END;
    i := i+1; j := j+1
  END
END MatchSeg;

PROCEDURE Match(patt, text: ARRAY OF CHAR): BOOLEAN;
  VAR i, j, t: INTEGER;
BEGIN
  IF ~MatchSeg(patt, 0, text, 0) THEN RETURN FALSE END;
  i := SegLen(patt, 0); j := i;
  WHILE patt[i] # 0X DO
    i := i+1;
    WHILE ~MatchSeg(patt, i, text, j) DO
      IF text[j] = 0X THEN RETURN FALSE END;
      j := j+1
    END;
    t := SegLen(patt, i);
    i := i+t; j := j+t
  END;
  RETURN TRUE
END Match;

PROCEDURE Test(patt, text: ARRAY OF CHAR; exp: BOOLEAN);
  VAR res: BOOLEAN;
BEGIN
  res := Match(patt, text);
  Out.Char('/'); Out.String(patt); Out.Char('/'); 
  Out.String(text); Out.Char('/'); 
  IF res THEN Out.String("TRUE") ELSE Out.String("FALSE") END;
  IF res # exp THEN Out.String(" (fail)") END;
  Out.Ln
END Test;

BEGIN
  Test("foo", "foo", TRUE);
  Test("foo", "foox", FALSE);
  Test("foo.*", "foo.c", TRUE);
  Test("foo.*", "foo", FALSE);
  Test("", "", TRUE);
  Test("x", "", FALSE);
  Test("", "x", FALSE);
END tWild.

(*<<
/foo/foo/TRUE
/foo/foox/FALSE
/foo.*/foo.c/TRUE
/foo.*/foo/FALSE
///TRUE
/x//FALSE
//x/FALSE
>>*)

(*[[
!! (SYMFILE #tWild STAMP #tWild.%main 1 #tWild.m)
!! (CHKSUM STAMP)
!! 
MODULE tWild STAMP 0
IMPORT Out STAMP
ENDHDR

PROC tWild.SegLen 4 3 0x00100001
! PROCEDURE SegLen(VAR patt: ARRAY OF CHAR; i: INTEGER): INTEGER;
!   k := 0;
CONST 0
STLW -4
LABEL L10
!   WHILE (patt[i+k] # 0X) & (patt[i+k] # '*') DO k := k+1 END;
LDLW 12
LDLW 20
LDLW -4
PLUS
LDLW 16
BOUND 9
LDIC
JEQZ L12
LDLW 12
LDLW 20
LDLW -4
PLUS
LDLW 16
BOUND 9
LDIC
CONST 42
JEQ L12
INCL -4
JUMP L10
LABEL L12
!   RETURN k
LDLW -4
RETURN
END

PROC tWild.MatchSeg 0 4 0x00900001
! PROCEDURE MatchSeg(VAR patt: ARRAY OF CHAR; i: INTEGER;
LABEL L14
!     CASE patt[i] OF
LDLW 12
LDLW 20
LDLW 16
BOUND 17
LDIC
CONST 63
TESTGEQ L27
CONST 42
TESTGEQ L28
JEQZ L20
JUMP L16
LABEL L28
CONST 42
JEQ L18
JUMP L16
LABEL L27
CONST 63
JEQ L19
JUMP L16
LABEL L18
!       '*': RETURN TRUE;
CONST 1
RETURN
LABEL L19
!     | '?': IF patt[i] = 0X THEN RETURN FALSE END
LDLW 12
LDLW 20
LDLW 16
BOUND 19
LDIC
JNEQZ L23
CONST 0
RETURN
LABEL L20
!     | 0X: RETURN (text[j] = 0X)
LDLW 24
LDLW 32
LDLW 28
BOUND 20
LDIC
CONST 0
EQ
RETURN
LABEL L16
!       IF patt[i] # text[j] THEN RETURN FALSE END
LDLW 12
LDLW 20
LDLW 16
BOUND 22
LDIC
LDLW 24
LDLW 32
LDLW 28
BOUND 22
LDIC
JEQ L23
CONST 0
RETURN
LABEL L23
!     i := i+1; j := j+1
INCL 20
INCL 32
JUMP L14
END

PROC tWild.Match 12 7 0
! PROCEDURE Match(patt, text: ARRAY OF CHAR): BOOLEAN;
LOCAL 12
LDLW 16
FLEXCOPY
LOCAL 20
LDLW 24
FLEXCOPY
!   IF ~MatchSeg(patt, 0, text, 0) THEN RETURN FALSE END;
CONST 0
LDLW 24
LDLW 20
CONST 0
LDLW 16
LDLW 12
GLOBAL tWild.MatchSeg
CALLW 6
JNEQZ L31
CONST 0
RETURN
LABEL L31
!   i := SegLen(patt, 0); j := i;
CONST 0
LDLW 16
LDLW 12
GLOBAL tWild.SegLen
CALLW 3
STLW -4
LDLW -4
STLW -8
LABEL L32
!   WHILE patt[i] # 0X DO
LDLW 12
LDLW -4
LDLW 16
BOUND 33
LDIC
JEQZ L34
!     i := i+1;
INCL -4
LABEL L35
!     WHILE ~MatchSeg(patt, i, text, j) DO
LDLW -8
LDLW 24
LDLW 20
LDLW -4
LDLW 16
LDLW 12
GLOBAL tWild.MatchSeg
CALLW 6
JNEQZ L37
!       IF text[j] = 0X THEN RETURN FALSE END;
LDLW 20
LDLW -8
LDLW 24
BOUND 36
LDIC
JNEQZ L40
CONST 0
RETURN
LABEL L40
!       j := j+1
INCL -8
JUMP L35
LABEL L37
!     t := SegLen(patt, i);
LDLW -4
LDLW 16
LDLW 12
GLOBAL tWild.SegLen
CALLW 3
STLW -12
!     i := i+t; j := j+t
LDLW -4
LDLW -12
PLUS
STLW -4
LDLW -8
LDLW -12
PLUS
STLW -8
JUMP L32
LABEL L34
!   RETURN TRUE
CONST 1
RETURN
END

PROC tWild.Test 4 5 0
! PROCEDURE Test(patt, text: ARRAY OF CHAR; exp: BOOLEAN);
LOCAL 12
LDLW 16
FLEXCOPY
LOCAL 20
LDLW 24
FLEXCOPY
!   res := Match(patt, text);
LDLW 24
LDLW 20
LDLW 16
LDLW 12
GLOBAL tWild.Match
CALLW 4
STLC -1
!   Out.Char('/'); Out.String(patt); Out.Char('/'); 
CONST 47
ALIGNC
GLOBAL Out.Char
CALL 1
LDLW 16
LDLW 12
GLOBAL Out.String
CALL 2
CONST 47
ALIGNC
GLOBAL Out.Char
CALL 1
!   Out.String(text); Out.Char('/'); 
LDLW 24
LDLW 20
GLOBAL Out.String
CALL 2
CONST 47
ALIGNC
GLOBAL Out.Char
CALL 1
!   IF res THEN Out.String("TRUE") ELSE Out.String("FALSE") END;
LDLC -1
JEQZ L43
CONST 5
GLOBAL tWild.%1
GLOBAL Out.String
CALL 2
JUMP L41
LABEL L43
CONST 6
GLOBAL tWild.%2
GLOBAL Out.String
CALL 2
LABEL L41
!   IF res # exp THEN Out.String(" (fail)") END;
LDLC -1
LDLC 28
JEQ L46
CONST 8
GLOBAL tWild.%3
GLOBAL Out.String
CALL 2
LABEL L46
!   Out.Ln
GLOBAL Out.Ln
CALL 0
RETURN
END

PROC tWild.%main 0 6 0
!   Test("foo", "foo", TRUE);
CONST 1
ALIGNC
CONST 4
GLOBAL tWild.%4
CONST 4
GLOBAL tWild.%4
GLOBAL tWild.Test
CALL 5
!   Test("foo", "foox", FALSE);
CONST 0
ALIGNC
CONST 5
GLOBAL tWild.%5
CONST 4
GLOBAL tWild.%4
GLOBAL tWild.Test
CALL 5
!   Test("foo.*", "foo.c", TRUE);
CONST 1
ALIGNC
CONST 6
GLOBAL tWild.%7
CONST 6
GLOBAL tWild.%6
GLOBAL tWild.Test
CALL 5
!   Test("foo.*", "foo", FALSE);
CONST 0
ALIGNC
CONST 4
GLOBAL tWild.%4
CONST 6
GLOBAL tWild.%6
GLOBAL tWild.Test
CALL 5
!   Test("", "", TRUE);
CONST 1
ALIGNC
CONST 1
GLOBAL tWild.%8
CONST 1
GLOBAL tWild.%8
GLOBAL tWild.Test
CALL 5
!   Test("x", "", FALSE);
CONST 0
ALIGNC
CONST 1
GLOBAL tWild.%8
CONST 2
GLOBAL tWild.%9
GLOBAL tWild.Test
CALL 5
!   Test("", "x", FALSE);
CONST 0
ALIGNC
CONST 2
GLOBAL tWild.%9
CONST 1
GLOBAL tWild.%8
GLOBAL tWild.Test
CALL 5
RETURN
END

! String "TRUE"
DEFINE tWild.%1
STRING 5452554500

! String "FALSE"
DEFINE tWild.%2
STRING 46414C534500

! String " (fail)"
DEFINE tWild.%3
STRING 20286661696C2900

! String "foo"
DEFINE tWild.%4
STRING 666F6F00

! String "foox"
DEFINE tWild.%5
STRING 666F6F7800

! String "foo.*"
DEFINE tWild.%6
STRING 666F6F2E2A00

! String "foo.c"
DEFINE tWild.%7
STRING 666F6F2E6300

! String ""
DEFINE tWild.%8
STRING 00

! String "x"
DEFINE tWild.%9
STRING 7800

! End of file
]]*)
