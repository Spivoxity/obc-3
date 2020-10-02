MODULE tGCD07;

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
END tGCD07.

(*[[
!! (SYMFILE #tGCD07 STAMP #tGCD07.%main 1 #tGCD07.m)
!! (CHKSUM STAMP)
!! 
MODULE tGCD07 STAMP 0
IMPORT Out STAMP
ENDHDR

PROC tGCD07.%main 0 3 0
!   x := 37 * 123; y := 57 * 123;
CONST 4551
STGW tGCD07.x
CONST 7011
STGW tGCD07.y
LABEL L1
!   WHILE x # y DO
LDGW tGCD07.x
LDGW tGCD07.y
JEQ L3
!     IF x > y THEN
LDGW tGCD07.x
LDGW tGCD07.y
JLEQ L6
!       x := x - y
LDGW tGCD07.x
LDGW tGCD07.y
MINUS
STGW tGCD07.x
JUMP L1
LABEL L6
!       y := y - x
LDGW tGCD07.y
LDGW tGCD07.x
MINUS
STGW tGCD07.y
JUMP L1
LABEL L3
!   Out.Int(x, 0); Out.Ln
CONST 0
LDGW tGCD07.x
GLOBAL Out.Int
CALL 2
GLOBAL Out.Ln
CALL 0
RETURN
END

! Global variables
GLOVAR tGCD07.x 4
GLOVAR tGCD07.y 4

! End of file
]]*)
