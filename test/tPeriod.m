MODULE tPeriod;

IMPORT Strings, Out;

PROCEDURE Period(CONST s: ARRAY OF CHAR): INTEGER;
  VAR a: POINTER TO ARRAY OF INTEGER;
    i, p, n: INTEGER;
BEGIN
  n := Strings.Length(s);
  NEW(a, n+1);
  p := -1; a[0] := p;
  FOR i := 0 TO n-1 DO
    WHILE (p >= 0) & (s[p] # s[i]) DO p := a[p] END;
    p := p+1; a[i+1] := p;
    Out.Int(i, 5); Out.Int(a[i+1], 5); Out.Char(' '); Out.Char(s[i]); Out.Ln
  END;
  RETURN n - a[n]
END Period;

BEGIN
  Out.Int(Period("RUDOLFRUDOLRRUDO"), 0); Out.Ln
END tPeriod.

(*<<
    0    0 R
    1    0 U
    2    0 D
    3    0 O
    4    0 L
    5    0 F
    6    1 R
    7    2 U
    8    3 D
    9    4 O
   10    5 L
   11    1 R
   12    1 R
   13    2 U
   14    3 D
   15    4 O
12
>>*)

(*[[
!! (SYMFILE #tPeriod STAMP #tPeriod.%main 1 #tPeriod.m)
!! (CHKSUM STAMP)
!! 
MODULE tPeriod STAMP 0
IMPORT Strings STAMP
IMPORT Out STAMP
ENDHDR

PROC tPeriod.Period 20 5 0x00110001
! PROCEDURE Period(CONST s: ARRAY OF CHAR): INTEGER;
!   n := Strings.Length(s);
LDLW 16
LDLW 12
GLOBAL Strings.Length
CALLW 2
STLW -16
!   NEW(a, n+1);
LDLW -16
INC
CONST 1
CONST 4
CONST 0
GLOBAL NEWFLEX
CALLW 4
STLW -4
!   p := -1; a[0] := p;
CONST -1
STLW -12
LDLW -12
LDLW -4
NCHECK 11
CONST 0
DUP 1
LDNW -4
LDNW 4
BOUND 11
STIW
!   FOR i := 0 TO n-1 DO
LDLW -16
DEC
STLW -20
CONST 0
STLW -8
LABEL L2
LDLW -8
LDLW -20
JGT L3
LABEL L4
!     WHILE (p >= 0) & (s[p] # s[i]) DO p := a[p] END;
LDLW -12
JLTZ L6
LDLW 12
LDLW -12
LDLW 16
BOUND 13
LDIC
LDLW 12
LDLW -8
LDLW 16
BOUND 13
LDIC
JEQ L6
LDLW -4
NCHECK 13
LDLW -12
DUP 1
LDNW -4
LDNW 4
BOUND 13
LDIW
STLW -12
JUMP L4
LABEL L6
!     p := p+1; a[i+1] := p;
INCL -12
LDLW -12
LDLW -4
NCHECK 14
LDLW -8
INC
DUP 1
LDNW -4
LDNW 4
BOUND 14
STIW
!     Out.Int(i, 5); Out.Int(a[i+1], 5); Out.Char(' '); Out.Char(s[i]); Out.Ln
CONST 5
LDLW -8
GLOBAL Out.Int
CALL 2
CONST 5
LDLW -4
NCHECK 15
LDLW -8
INC
DUP 1
LDNW -4
LDNW 4
BOUND 15
LDIW
GLOBAL Out.Int
CALL 2
CONST 32
ALIGNC
GLOBAL Out.Char
CALL 1
LDLW 12
LDLW -8
LDLW 16
BOUND 15
LDIC
ALIGNC
GLOBAL Out.Char
CALL 1
GLOBAL Out.Ln
CALL 0
!   FOR i := 0 TO n-1 DO
INCL -8
JUMP L2
LABEL L3
!   RETURN n - a[n]
LDLW -16
LDLW -4
NCHECK 17
LDLW -16
DUP 1
LDNW -4
LDNW 4
BOUND 17
LDIW
MINUS
RETURN
END

PROC tPeriod.%main 0 4 0
!   Out.Int(Period("RUDOLFRUDOLRRUDO"), 0); Out.Ln
CONST 0
CONST 17
GLOBAL tPeriod.%1
GLOBAL tPeriod.Period
CALLW 2
GLOBAL Out.Int
CALL 2
GLOBAL Out.Ln
CALL 0
RETURN
END

! String "RUDOLFRUDOLRRUDO"
DEFINE tPeriod.%1
STRING 5255444F4C465255444F4C525255444F00

! End of file
]]*)
