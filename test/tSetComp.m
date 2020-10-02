MODULE tSetComp;

VAR x, y: SET; b: BOOLEAN; n: INTEGER;

BEGIN
  b := (x <= y);
  b := (y >= {n});
  b := ({3} <= y)
END tSetComp.

(*[[
!! (SYMFILE #tSetComp STAMP #tSetComp.%main 1 #tSetComp.m)
!! (CHKSUM STAMP)
!! 
MODULE tSetComp STAMP 0
ENDHDR

PROC tSetComp.%main 0 3 0
!   b := (x <= y);
LDGW tSetComp.x
LDGW tSetComp.y
BITNOT
BITAND
CONST 0
EQ
STGC tSetComp.b
!   b := (y >= {n});
CONST 1
LDGW tSetComp.n
CONST 32
BOUND 7
LSL
LDGW tSetComp.y
BITNOT
BITAND
CONST 0
EQ
STGC tSetComp.b
!   b := ({3} <= y)
LDGW tSetComp.y
BITNOT
CONST 8
BITAND
CONST 0
EQ
STGC tSetComp.b
RETURN
END

! Global variables
GLOVAR tSetComp.x 4
GLOVAR tSetComp.y 4
GLOVAR tSetComp.b 1
GLOVAR tSetComp.n 4

! End of file
]]*)
