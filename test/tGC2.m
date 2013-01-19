MODULE tGC2;

(*<<
500500
2097152
>>*)

IMPORT Out, GC;

TYPE ptr = POINTER TO cell;
  cell = RECORD hd: INTEGER; tl: ptr END;

PROCEDURE Reverse(a: ptr): ptr;
BEGIN
  IF a = NIL THEN
    RETURN NIL
  ELSE
    RETURN Snoc(Reverse(a.tl), a.hd)
  END
END Reverse;

PROCEDURE Snoc(a: ptr; x: INTEGER): ptr;
  VAR r: ptr;
BEGIN
  NEW(r);
  IF a = NIL THEN
    r.hd := x; r.tl := NIL
  ELSE
    r.hd := a.hd; r.tl := Snoc(a.tl, x)
  END;
  RETURN r
END Snoc;

PROCEDURE Sum(a: ptr): INTEGER;
BEGIN
  IF a = NIL THEN
    RETURN 0
  ELSE
    RETURN a.hd + Sum(a.tl)
  END
END Sum;

VAR n: INTEGER; a: ptr;

BEGIN
  a := NIL;
  FOR n := 1 TO 1000 DO a := Snoc(a, n) END;
  Out.Int(Sum(Reverse(a)), 0); Out.Ln;
  Out.Int(GC.HeapSize(), 0); Out.Ln
END tGC2.

(*[[
!! SYMFILE #tGC2 STAMP #tGC2.%main 1
!! END STAMP
!! 
MODULE tGC2 STAMP 0
IMPORT Out STAMP
IMPORT GC STAMP
ENDHDR

PROC tGC2.Reverse 0 3 0x00100001
! PROCEDURE Reverse(a: ptr): ptr;
!   IF a = NIL THEN
LDLW 12
JNEQZ 2
!     RETURN NIL
CONST 0
RETURNW
LABEL 2
!     RETURN Snoc(Reverse(a.tl), a.hd)
LDLW 12
NCHECK 18
LOADW
LDLW 12
NCHECK 18
LDNW 4
GLOBAL tGC2.Reverse
CALLW 1
GLOBAL tGC2.Snoc
CALLW 2
RETURNW
END

PROC tGC2.Snoc 4 4 0x00110001
! PROCEDURE Snoc(a: ptr; x: INTEGER): ptr;
!   NEW(r);
CONST 8
GLOBAL tGC2.cell
LOCAL -4
GLOBAL NEW
CALL 3
!   IF a = NIL THEN
LDLW 12
JNEQZ 4
!     r.hd := x; r.tl := NIL
LDLW 16
LDLW -4
NCHECK 27
STOREW
CONST 0
LDLW -4
NCHECK 27
STNW 4
JUMP 3
LABEL 4
!     r.hd := a.hd; r.tl := Snoc(a.tl, x)
LDLW 12
NCHECK 29
LOADW
LDLW -4
NCHECK 29
STOREW
LDLW 16
LDLW 12
NCHECK 29
LDNW 4
GLOBAL tGC2.Snoc
CALLW 2
LDLW -4
NCHECK 29
STNW 4
LABEL 3
!   RETURN r
LDLW -4
RETURNW
END

PROC tGC2.Sum 0 4 0x00100001
! PROCEDURE Sum(a: ptr): INTEGER;
!   IF a = NIL THEN
LDLW 12
JNEQZ 6
!     RETURN 0
CONST 0
RETURNW
LABEL 6
!     RETURN a.hd + Sum(a.tl)
LDLW 12
NCHECK 39
LOADW
LDLW 12
NCHECK 39
LDNW 4
GLOBAL tGC2.Sum
CALLW 1
PLUS
RETURNW
END

PROC tGC2.%main 0 4 0
!   a := NIL;
CONST 0
STGW tGC2.a
!   FOR n := 1 TO 1000 DO a := Snoc(a, n) END;
CONST 1
STGW tGC2.n
LABEL 7
LDGW tGC2.n
CONST 1000
JGT 8
LDGW tGC2.n
LDGW tGC2.a
GLOBAL tGC2.Snoc
CALLW 2
STGW tGC2.a
LDGW tGC2.n
INC
STGW tGC2.n
JUMP 7
LABEL 8
!   Out.Int(Sum(Reverse(a)), 0); Out.Ln;
CONST 0
LDGW tGC2.a
GLOBAL tGC2.Reverse
CALLW 1
GLOBAL tGC2.Sum
CALLW 1
GLOBAL Out.Int
CALL 2
GLOBAL Out.Ln
CALL 0
!   Out.Int(GC.HeapSize(), 0); Out.Ln
CONST 0
GLOBAL GC.HeapSize
CALLW 0
GLOBAL Out.Int
CALL 2
GLOBAL Out.Ln
CALL 0
RETURN
END

! Global variables
GLOVAR tGC2.n 4
GLOVAR tGC2.a 4

! Pointer map
DEFINE tGC2.%gcmap
WORD GC_BASE
WORD tGC2.a
WORD 0
WORD GC_END

! Descriptor for cell
DEFINE tGC2.cell
WORD 0x00000005
WORD 0
WORD tGC2.cell.%anc

DEFINE tGC2.cell.%anc
WORD tGC2.cell

! End of file
]]*)
