MODULE tPow;

IMPORT Out;

TYPE Box = POINTER TO RECORD value: INTEGER END;

PROCEDURE box(x: INTEGER): Box;
  VAR p: Box;
BEGIN
  NEW(p);
  p.value := x;
  RETURN p
END box;

PROCEDURE Plus(x, y: Box): Box;
BEGIN
  RETURN box(x.value + y.value);
END Plus;

PROCEDURE Minus(x, y: Box): Box;
BEGIN
  RETURN box(x.value - y.value)
END Minus;

PROCEDURE PTwo(n: Box): Box;
BEGIN
  IF n.value = 0 THEN
    RETURN box(1)
  ELSE
    RETURN Plus(PTwo(Minus(n, box(1))), PTwo(Minus(n, box(1))))
  END;
END PTwo;

VAR q: Box;

BEGIN
  q := PTwo(box(17));
  Out.Int(q.value, 0);
  Out.Ln
END tPow.

(*<<
131072
>>*)

(*[[
!! (SYMFILE #tPow STAMP #tPow.%main 1 #tPow.m)
!! (CHKSUM STAMP)
!! 
MODULE tPow STAMP 0
IMPORT Out STAMP
ENDHDR

PROC tPow.box 4 3 0x00010001
! PROCEDURE box(x: INTEGER): Box;
!   NEW(p);
CONST 4
GLOBAL tPow.%1
GLOBAL NEW
CALLW 2
STLW -4
!   p.value := x;
LDLW 12
LDLW -4
NCHECK 11
STOREW
!   RETURN p
LDLW -4
RETURN
END

PROC tPow.Plus 0 3 0x00300001
! PROCEDURE Plus(x, y: Box): Box;
!   RETURN box(x.value + y.value);
LDLW 12
NCHECK 17
LOADW
LDLW 16
NCHECK 17
LOADW
PLUS
GLOBAL tPow.box
CALLW 1
RETURN
END

PROC tPow.Minus 0 3 0x00300001
! PROCEDURE Minus(x, y: Box): Box;
!   RETURN box(x.value - y.value)
LDLW 12
NCHECK 22
LOADW
LDLW 16
NCHECK 22
LOADW
MINUS
GLOBAL tPow.box
CALLW 1
RETURN
END

PROC tPow.PTwo 0 4 0x00100001
! PROCEDURE PTwo(n: Box): Box;
!   IF n.value = 0 THEN
LDLW 12
NCHECK 27
LOADW
JNEQZ L4
!     RETURN box(1)
CONST 1
GLOBAL tPow.box
CALLW 1
RETURN
LABEL L4
!     RETURN Plus(PTwo(Minus(n, box(1))), PTwo(Minus(n, box(1))))
CONST 1
GLOBAL tPow.box
CALLW 1
LDLW 12
GLOBAL tPow.Minus
CALLW 2
GLOBAL tPow.PTwo
CALLW 1
CONST 1
GLOBAL tPow.box
STKMAP 0x00000005
CALLW 1
LDLW 12
GLOBAL tPow.Minus
STKMAP 0x00000009
CALLW 2
GLOBAL tPow.PTwo
STKMAP 0x00000005
CALLW 1
GLOBAL tPow.Plus
CALLW 2
RETURN
END

PROC tPow.%main 0 3 0
!   q := PTwo(box(17));
CONST 17
GLOBAL tPow.box
CALLW 1
GLOBAL tPow.PTwo
CALLW 1
STGW tPow.q
!   Out.Int(q.value, 0);
CONST 0
LDGW tPow.q
NCHECK 38
LOADW
GLOBAL Out.Int
CALL 2
!   Out.Ln
GLOBAL Out.Ln
CALL 0
RETURN
END

! Global variables
GLOVAR tPow.q 4

! Global pointer map
DEFINE tPow.%gcmap
WORD GC_POINTER
WORD tPow.q
WORD GC_END

! Descriptor for *anon*
DEFINE tPow.%1
WORD 0
WORD 0
WORD tPow.%1.%anc

DEFINE tPow.%1.%anc
WORD tPow.%1

! End of file
]]*)
