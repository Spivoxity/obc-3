MODULE tSetComp;

VAR x, y: SET; b: BOOLEAN; n: INTEGER;

BEGIN
  b := (x <= y);
  b := ({n} <= y);
  b := ({3} <= y)
END tSetComp.

(*[[
!! SYMFILE #tSetComp STAMP #tSetComp.%main 1
!! END STAMP
!! 
MODULE tSetComp STAMP 0
ENDHDR

PROC tSetComp.%main 0 8 0
!   b := (x <= y);
LDGW tSetComp.x
LDGW tSetComp.y
BITNOT
BITAND
CONST 0
EQ
STGC tSetComp.b
!   b := ({n} <= y);
LDGW tSetComp.n
CONST 32
BOUND 7
BIT
LDGW tSetComp.y
BITNOT
BITAND
CONST 0
EQ
STGC tSetComp.b
!   b := ({3} <= y)
CONST 8
LDGW tSetComp.y
BITNOT
BITAND
CONST 0
EQ
STGC tSetComp.b
RETURN
END

! Global variables
GLOBAL tSetComp.x 4
GLOBAL tSetComp.y 4
GLOBAL tSetComp.b 1
GLOBAL tSetComp.n 4

! End of file
]]*)
