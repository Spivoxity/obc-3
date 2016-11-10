MODULE tDigits;

(*<<
381654729
>>*)

IMPORT Out, GC;

TYPE SetRec = ABSTRACT RECORD END;
  Set = POINTER TO SetRec;
  UniSet = POINTER TO RECORD (SetRec) END;
  DelSet = POINTER TO RECORD (SetRec) del: INTEGER; parent: Set; END;

ABSTRACT PROCEDURE (r: Set) Avail(x: INTEGER): BOOLEAN;

PROCEDURE (r: UniSet) Avail(x: INTEGER): BOOLEAN;
BEGIN RETURN TRUE END Avail;

PROCEDURE (r: DelSet) Avail(x: INTEGER): BOOLEAN;
BEGIN RETURN (x # r.del) & r.parent.Avail(x) END Avail;

PROCEDURE Search(k, n: INTEGER; s: Set);
  VAR d, nn: INTEGER; s1: DelSet; b: BOOLEAN;
BEGIN
  IF k = 10 THEN
    GC.Collect;
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
END tDigits.

(*[[
!! SYMFILE #tDigits STAMP #tDigits.%main 1
!! END STAMP
!! 
MODULE tDigits STAMP 0
IMPORT Out STAMP
IMPORT GC STAMP
ENDHDR

PROC tDigits.%2.Avail 0 1 0x00100001
! PROCEDURE (r: UniSet) Avail(x: INTEGER): BOOLEAN;
! BEGIN RETURN TRUE END Avail;
CONST 1
RETURNW
END

PROC tDigits.%1.Avail 0 4 0x00100001
! PROCEDURE (r: DelSet) Avail(x: INTEGER): BOOLEAN;
! BEGIN RETURN (x # r.del) & r.parent.Avail(x) END Avail;
LDLW 16
LDLW 12
NCHECK 20
LOADW
JNEQ 3
CONST 0
RETURNW
LABEL 3
LDLW 16
LDLW 12
NCHECK 20
LDNW 4
NCHECK 20
DUP 0
LDNW -4
LDNW 12
CALLW 2
RETURNW
END

PROC tDigits.Search 4 4 0x00404001
! PROCEDURE Search(k, n: INTEGER; s: Set);
!   IF k = 10 THEN
LDLW 12
CONST 10
JNEQ 6
!     GC.Collect;
GLOBAL GC.Collect
CALL 0
!     Out.Int(n, 0); Out.Ln
CONST 0
LDLW 16
GLOBAL Out.Int
CALL 2
GLOBAL Out.Ln
CALL 0
RETURN
LABEL 6
!     FOR d := 1 TO 9 DO
CONST 1
STLW -4
JUMP 8
LABEL 7
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
JNEQZ 10
LDLW -4
LDLW 20
NCHECK 31
DUP 0
LDNW -4
LDNW 12
CALLW 2
JUMPF 10
! 	b := s IS DelSet;
LDLW 20
NCHECK 32
LDNW -4
GLOBAL tDigits.%1
TYPETEST 1
STLC -13
! 	NEW(s1); s1.del := d; s1.parent := s;
CONST 8
GLOBAL tDigits.%1
LOCAL -12
GLOBAL NEW
CALL 3
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
GLOBAL tDigits.Search
CALL 3
LABEL 10
!     FOR d := 1 TO 9 DO
INCL -4
LABEL 8
LDLW -4
CONST 9
JLEQ 7
RETURN
END

PROC tDigits.Main 1 4 0x00010001
! PROCEDURE Main;
!   NEW(s);
CONST 0
GLOBAL tDigits.%2
LOCAL -4
GLOBAL NEW
CALL 3
!   Search(1, 0, s)
LDLW -4
CONST 0
CONST 1
GLOBAL tDigits.Search
CALL 3
RETURN
END

PROC tDigits.%main 0 4 0
!   Main
GLOBAL tDigits.Main
CALL 0
RETURN
END

! Descriptor for SetRec
DEFINE tDigits.SetRec
WORD 0

! Descriptor for *anon*
DEFINE tDigits.%1
WORD 0x00000005
WORD 1
WORD tDigits.%1.%anc
WORD tDigits.%1.Avail

DEFINE tDigits.%1.%anc
WORD tDigits.SetRec
WORD tDigits.%1

! Descriptor for *anon*
DEFINE tDigits.%2
WORD 0
WORD 1
WORD tDigits.%2.%anc
WORD tDigits.%2.Avail

DEFINE tDigits.%2.%anc
WORD tDigits.SetRec
WORD tDigits.%2

! End of file
]]*)
