MODULE tPow2;

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

PROCEDURE GCdebug(flags: ARRAY OF CHAR) IS "gc_debug";

BEGIN
  GCdebug("sz");
  q := PTwo(box(1));
  Out.Int(q.value, 0);
  Out.Ln
END tPow2.

(*<<
2
>>*)

(*[[
!! (SYMFILE #tPow2 STAMP #tPow2.%main 1 #tPow2.m)
!! (CHKSUM STAMP)
!! 
MODULE tPow2 STAMP 0
IMPORT Out STAMP
ENDHDR

PROC tPow2.box 4 3 0x00010001
! PROCEDURE box(x: INTEGER): Box;
!   NEW(p);
CONST 4
GLOBAL tPow2.%2
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

PROC tPow2.Plus 0 3 0x00300001
! PROCEDURE Plus(x, y: Box): Box;
!   RETURN box(x.value + y.value);
LDLW 12
NCHECK 17
LOADW
LDLW 16
NCHECK 17
LOADW
PLUS
GLOBAL tPow2.box
CALLW 1
RETURN
END

PROC tPow2.Minus 0 3 0x00300001
! PROCEDURE Minus(x, y: Box): Box;
!   RETURN box(x.value - y.value)
LDLW 12
NCHECK 22
LOADW
LDLW 16
NCHECK 22
LOADW
MINUS
GLOBAL tPow2.box
CALLW 1
RETURN
END

PROC tPow2.PTwo 0 4 0x00100001
! PROCEDURE PTwo(n: Box): Box;
!   IF n.value = 0 THEN
LDLW 12
NCHECK 27
LOADW
JNEQZ L5
!     RETURN box(1)
CONST 1
GLOBAL tPow2.box
CALLW 1
RETURN
LABEL L5
!     RETURN Plus(PTwo(Minus(n, box(1))), PTwo(Minus(n, box(1))))
CONST 1
GLOBAL tPow2.box
CALLW 1
LDLW 12
GLOBAL tPow2.Minus
CALLW 2
GLOBAL tPow2.PTwo
CALLW 1
CONST 1
GLOBAL tPow2.box
STKMAP 0x00000005
CALLW 1
LDLW 12
GLOBAL tPow2.Minus
STKMAP 0x00000009
CALLW 2
GLOBAL tPow2.PTwo
STKMAP 0x00000005
CALLW 1
GLOBAL tPow2.Plus
CALLW 2
RETURN
END

PRIMDEF tPow2.GCdebug gc_debug VX

PROC tPow2.%main 0 3 0
!   GCdebug("sz");
CONST 3
GLOBAL tPow2.%1
GLOBAL tPow2.GCdebug
CALL 2
!   q := PTwo(box(1));
CONST 1
GLOBAL tPow2.box
CALLW 1
GLOBAL tPow2.PTwo
CALLW 1
STGW tPow2.q
!   Out.Int(q.value, 0);
CONST 0
LDGW tPow2.q
NCHECK 41
LOADW
GLOBAL Out.Int
CALL 2
!   Out.Ln
GLOBAL Out.Ln
CALL 0
RETURN
END

! Global variables
GLOVAR tPow2.q 4

! Global pointer map
DEFINE tPow2.%gcmap
WORD GC_POINTER
WORD tPow2.q
WORD GC_END

! String "sz"
DEFINE tPow2.%1
STRING 737A00

! Descriptor for *anon*
DEFINE tPow2.%2
WORD 0
WORD 0
WORD tPow2.%2.%anc

DEFINE tPow2.%2.%anc
WORD tPow2.%2

! End of file
]]*)
