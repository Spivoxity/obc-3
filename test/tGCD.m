MODULE tGCD;

(*<<
123
>>*)

IMPORT Out;

VAR x, y: INTEGER;

BEGIN
  x := 37 * 123; y := 57 * 123;
  WHILE x # y DO
    IF x > y THEN
      x := x - y
    ELSE
      y := y - x
    END
  END;
  Out.Int(x, 0); Out.Ln
END tGCD.

(*[[
!! (SYMFILE #tGCD STAMP #tGCD.%main 1 #tGCD.m)
!! (CHKSUM STAMP)
!! 
MODULE tGCD STAMP 0
IMPORT Out STAMP
ENDHDR

PROC tGCD.%main 0 3 0
!   x := 37 * 123; y := 57 * 123;
CONST 4551
STGW tGCD.x
CONST 7011
STGW tGCD.y
LABEL L1
!   WHILE x # y DO
LDGW tGCD.x
LDGW tGCD.y
JEQ L3
!     IF x > y THEN
LDGW tGCD.x
LDGW tGCD.y
JLEQ L6
!       x := x - y
LDGW tGCD.x
LDGW tGCD.y
MINUS
STGW tGCD.x
JUMP L1
LABEL L6
!       y := y - x
LDGW tGCD.y
LDGW tGCD.x
MINUS
STGW tGCD.y
JUMP L1
LABEL L3
!   Out.Int(x, 0); Out.Ln
CONST 0
LDGW tGCD.x
GLOBAL Out.Int
CALL 2
GLOBAL Out.Ln
CALL 0
RETURN
END

! Global variables
GLOVAR tGCD.x 4
GLOVAR tGCD.y 4

! End of file
]]*)
