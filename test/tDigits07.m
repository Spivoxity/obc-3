MODULE tDigits07;

(*<<
381654729
>>*)

IMPORT Out, SYSTEM;

TYPE SetRec = ABSTRACT RECORD END;
  Set = POINTER TO SetRec;
  UniSet = POINTER TO RECORD (SetRec) END;
  DelSet = POINTER TO RECORD (SetRec) del: INTEGER; parent: Set END;

ABSTRACT PROCEDURE (r: Set) Avail(x: INTEGER): BOOLEAN;

PROCEDURE (r: UniSet) Avail(x: INTEGER): BOOLEAN;
BEGIN RETURN TRUE END Avail;

PROCEDURE (r: DelSet) Avail(x: INTEGER): BOOLEAN;
BEGIN RETURN (x # r.del) & r.parent.Avail(x) END Avail;

PROCEDURE Search(k, n: INTEGER; s: Set);
  VAR d, nn: INTEGER; s1: DelSet; b: BOOLEAN;
BEGIN
  IF k = 10 THEN
    SYSTEM.GC;
    Out.Int(n, 0); Out.Ln
  ELSE
    FOR d := 1 TO 9 DO
      nn := 10 * n + d;
      IF (nn MOD k = 0) & s.Avail(d) THEN
	b := s IS DelSet;
	NEW(s1); s1.del := d; s1.parent := s;
        Search(k+1, nn, s1)
      END
    END
  END
END Search;

PROCEDURE Main;
VAR s: UniSet;
BEGIN
  NEW(s);
  Search(1, 0, s)
END Main;

BEGIN
  Main
END tDigits07.

(*[[
!! (SYMFILE #tDigits07 STAMP #tDigits07.%main 1 #tDigits07.m)
!! (CHKSUM STAMP)
!! 
MODULE tDigits07 STAMP 0
IMPORT Out STAMP
ENDHDR

PROC tDigits07.%1.Avail 0 1 0x00100001
! PROCEDURE (r: UniSet) Avail(x: INTEGER): BOOLEAN;
! BEGIN RETURN TRUE END Avail;
CONST 1
RETURN
END

PROC tDigits07.%2.Avail 0 4 0x00100001
! PROCEDURE (r: DelSet) Avail(x: INTEGER): BOOLEAN;
! BEGIN RETURN (x # r.del) & r.parent.Avail(x) END Avail;
LDLW 16
LDLW 12
NCHECK 20
LOADW
JNEQ L4
CONST 0
RETURN
LABEL L4
LDLW 16
LDLW 12
NCHECK 20
LDNW 4
NCHECK 20
DUP 0
LDNW -4
LDNW 12
CALLW 2
RETURN
END

PROC tDigits07.Search 16 4 0x00404001
! PROCEDURE Search(k, n: INTEGER; s: Set);
!   IF k = 10 THEN
LDLW 12
CONST 10
JNEQ L18
!     SYSTEM.GC;
GLOBAL SYSTEM.GC
CALL 0
!     Out.Int(n, 0); Out.Ln
CONST 0
LDLW 16
GLOBAL Out.Int
CALL 2
GLOBAL Out.Ln
CALL 0
RETURN
LABEL L18
!     FOR d := 1 TO 9 DO
CONST 1
STLW -4
LABEL L7
LDLW -4
CONST 9
JGT L8
!       nn := 10 * n + d;
LDLW 16
CONST 10
TIMES
LDLW -4
PLUS
STLW -8
!       IF (nn MOD k = 0) & s.Avail(d) THEN
LDLW -8
LDLW 12
ZCHECK 31
MOD
JNEQZ L11
LDLW -4
LDLW 20
NCHECK 31
DUP 0
LDNW -4
LDNW 12
CALLW 2
JEQZ L11
! 	b := s IS DelSet;
LDLW 20
NCHECK 32
LDNW -4
DUP 0
LDNW 4
CONST 1
JGEQ L15
POP 1
JUMP L13
LABEL L15
LDNW 8
LDNW 4
GLOBAL tDigits07.%2
JNEQ L13
CONST 1
JUMP L14
LABEL L13
CONST 0
LABEL L14
STLC -13
! 	NEW(s1); s1.del := d; s1.parent := s;
CONST 8
GLOBAL tDigits07.%2
GLOBAL NEW
CALLW 2
STLW -12
LDLW -4
LDLW -12
NCHECK 33
STOREW
LDLW 20
LDLW -12
NCHECK 33
STNW 4
!         Search(k+1, nn, s1)
LDLW -12
LDLW -8
LDLW 12
INC
GLOBAL tDigits07.Search
CALL 3
LABEL L11
!     FOR d := 1 TO 9 DO
INCL -4
JUMP L7
LABEL L8
RETURN
END

PROC tDigits07.Main 4 4 0x00010001
! PROCEDURE Main;
!   NEW(s);
CONST 0
GLOBAL tDigits07.%1
GLOBAL NEW
CALLW 2
STLW -4
!   Search(1, 0, s)
LDLW -4
CONST 0
CONST 1
GLOBAL tDigits07.Search
CALL 3
RETURN
END

PROC tDigits07.%main 0 1 0
!   Main
GLOBAL tDigits07.Main
CALL 0
RETURN
END

! Descriptor for SetRec
DEFINE tDigits07.SetRec
WORD 0

! Descriptor for *anon*
DEFINE tDigits07.%1
WORD 0
WORD 1
WORD tDigits07.%1.%anc
WORD tDigits07.%1.Avail

DEFINE tDigits07.%1.%anc
WORD tDigits07.SetRec
WORD tDigits07.%1

! Descriptor for *anon*
DEFINE tDigits07.%2
WORD 0x00000005
WORD 1
WORD tDigits07.%2.%anc
WORD tDigits07.%2.Avail

DEFINE tDigits07.%2.%anc
WORD tDigits07.SetRec
WORD tDigits07.%2

! End of file
]]*)
