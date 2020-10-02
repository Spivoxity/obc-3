MODULE tGC2;

(*<<
500500
>>*)

IMPORT Out;

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
END tGC2.

(*[[
!! (SYMFILE #tGC2 STAMP #tGC2.%main 1 #tGC2.m)
!! (CHKSUM STAMP)
!! 
MODULE tGC2 STAMP 0
IMPORT Out STAMP
ENDHDR

PROC tGC2.Reverse 0 3 0x00100001
! PROCEDURE Reverse(a: ptr): ptr;
!   IF a = NIL THEN
LDLW 12
JNEQZ L3
!     RETURN NIL
CONST 0
RETURN
LABEL L3
!     RETURN Snoc(Reverse(a.tl), a.hd)
LDLW 12
NCHECK 17
LOADW
LDLW 12
NCHECK 17
LDNW 4
GLOBAL tGC2.Reverse
CALLW 1
GLOBAL tGC2.Snoc
CALLW 2
RETURN
END

PROC tGC2.Snoc 4 3 0x00110001
! PROCEDURE Snoc(a: ptr; x: INTEGER): ptr;
!   NEW(r);
CONST 8
GLOBAL tGC2.cell
GLOBAL NEW
CALLW 2
STLW -4
!   IF a = NIL THEN
LDLW 12
JNEQZ L6
!     r.hd := x; r.tl := NIL
LDLW 16
LDLW -4
NCHECK 26
STOREW
CONST 0
LDLW -4
NCHECK 26
STNW 4
JUMP L4
LABEL L6
!     r.hd := a.hd; r.tl := Snoc(a.tl, x)
LDLW 12
NCHECK 28
LOADW
LDLW -4
NCHECK 28
STOREW
LDLW 16
LDLW 12
NCHECK 28
LDNW 4
GLOBAL tGC2.Snoc
CALLW 2
LDLW -4
NCHECK 28
STNW 4
LABEL L4
!   RETURN r
LDLW -4
RETURN
END

PROC tGC2.Sum 0 3 0x00100001
! PROCEDURE Sum(a: ptr): INTEGER;
!   IF a = NIL THEN
LDLW 12
JNEQZ L9
!     RETURN 0
CONST 0
RETURN
LABEL L9
!     RETURN a.hd + Sum(a.tl)
LDLW 12
NCHECK 38
LOADW
LDLW 12
NCHECK 38
LDNW 4
GLOBAL tGC2.Sum
CALLW 1
PLUS
RETURN
END

PROC tGC2.%main 0 3 0
!   a := NIL;
CONST 0
STGW tGC2.a
!   FOR n := 1 TO 1000 DO a := Snoc(a, n) END;
CONST 1
STGW tGC2.n
LABEL L10
LDGW tGC2.n
CONST 1000
JGT L11
LDGW tGC2.n
LDGW tGC2.a
GLOBAL tGC2.Snoc
CALLW 2
STGW tGC2.a
LDGW tGC2.n
INC
STGW tGC2.n
JUMP L10
LABEL L11
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
RETURN
END

! Global variables
GLOVAR tGC2.n 4
GLOVAR tGC2.a 4

! Global pointer map
DEFINE tGC2.%gcmap
WORD GC_POINTER
WORD tGC2.a
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
