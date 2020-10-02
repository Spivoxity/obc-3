MODULE tSetComp07;

VAR x, y: SET; b: BOOLEAN; n: INTEGER;

BEGIN
  b := (x <= y);
  b := ({n} <= y);
  b := ({3} <= y)
END tSetComp07.

(*[[
!! (SYMFILE #tSetComp07 STAMP #tSetComp07.%main 1 #tSetComp07.m)
!! (CHKSUM STAMP)
!! 
MODULE tSetComp07 STAMP 0
ENDHDR

PROC tSetComp07.%main 0 3 0
!   b := (x <= y);
LDGW tSetComp07.x
LDGW tSetComp07.y
BITNOT
BITAND
CONST 0
EQ
STGC tSetComp07.b
!   b := ({n} <= y);
CONST 1
LDGW tSetComp07.n
CONST 32
BOUND 7
LSL
LDGW tSetComp07.y
BITNOT
BITAND
CONST 0
EQ
STGC tSetComp07.b
!   b := ({3} <= y)
LDGW tSetComp07.y
BITNOT
CONST 8
BITAND
CONST 0
EQ
STGC tSetComp07.b
RETURN
END

! Global variables
GLOVAR tSetComp07.x 4
GLOVAR tSetComp07.y 4
GLOVAR tSetComp07.b 1
GLOVAR tSetComp07.n 4

! End of file
]]*)
