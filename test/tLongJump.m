MODULE tLongJump;

(*<<
 1 101 201 301 351 401 451 501 502 505
>>*)

IMPORT Out;

VAR x: INTEGER;

BEGIN
  x := 0;
  REPEAT
    CASE x OF
    502:
      x := x+3
    | 1..300:
      x := x + 1; x := x + 1; x := x + 1; x := x + 1; x := x + 1; 
      x := x + 1; x := x + 1; x := x + 1; x := x + 1; x := x + 1; 
      x := x + 1; x := x + 1; x := x + 1; x := x + 1; x := x + 1; 
      x := x + 1; x := x + 1; x := x + 1; x := x + 1; x := x + 1; 
      x := x + 1; x := x + 1; x := x + 1; x := x + 1; x := x + 1; 

      x := x + 1; x := x + 1; x := x + 1; x := x + 1; x := x + 1; 
      x := x + 1; x := x + 1; x := x + 1; x := x + 1; x := x + 1; 
      x := x + 1; x := x + 1; x := x + 1; x := x + 1; x := x + 1; 
      x := x + 1; x := x + 1; x := x + 1; x := x + 1; x := x + 1; 
      x := x + 1; x := x + 1; x := x + 1; x := x + 1; x := x + 1; 

      x := x + 1; x := x + 1; x := x + 1; x := x + 1; x := x + 1; 
      x := x + 1; x := x + 1; x := x + 1; x := x + 1; x := x + 1; 
      x := x + 1; x := x + 1; x := x + 1; x := x + 1; x := x + 1; 
      x := x + 1; x := x + 1; x := x + 1; x := x + 1; x := x + 1; 
      x := x + 1; x := x + 1; x := x + 1; x := x + 1; x := x + 1; 

      x := x + 1; x := x + 1; x := x + 1; x := x + 1; x := x + 1; 
      x := x + 1; x := x + 1; x := x + 1; x := x + 1; x := x + 1; 
      x := x + 1; x := x + 1; x := x + 1; x := x + 1; x := x + 1; 
      x := x + 1; x := x + 1; x := x + 1; x := x + 1; x := x + 1; 
      x := x + 1; x := x + 1; x := x + 1; x := x + 1; x := x + 1
    | 301..499:
      x := x+50
    ELSE
      x := x+1
    END;

    Out.Char(' '); Out.Int(x,0)
  UNTIL x >= 503;
  Out.Ln
END tLongJump.

(*[[
!! (SYMFILE #tLongJump STAMP #tLongJump.%main 1 #tLongJump.m)
!! (CHKSUM STAMP)
!! 
MODULE tLongJump STAMP 0
IMPORT Out STAMP
ENDHDR

PROC tLongJump.%main 0 3 0
!   x := 0;
CONST 0
STGW tLongJump.x
LABEL L1
!     CASE x OF
LDGW tLongJump.x
CONST 502
TESTGEQ L8
CONST 301
TESTGEQ L9
CONST 1
JGEQ L6
JUMP L3
LABEL L9
CONST 499
JLEQ L7
JUMP L3
LABEL L8
CONST 502
JNEQ L3
!       x := x+3
LDGW tLongJump.x
CONST 3
PLUS
STGW tLongJump.x
JUMP L4
LABEL L6
!       x := x + 1; x := x + 1; x := x + 1; x := x + 1; x := x + 1; 
LDGW tLongJump.x
INC
STGW tLongJump.x
LDGW tLongJump.x
INC
STGW tLongJump.x
LDGW tLongJump.x
INC
STGW tLongJump.x
LDGW tLongJump.x
INC
STGW tLongJump.x
LDGW tLongJump.x
INC
STGW tLongJump.x
!       x := x + 1; x := x + 1; x := x + 1; x := x + 1; x := x + 1; 
LDGW tLongJump.x
INC
STGW tLongJump.x
LDGW tLongJump.x
INC
STGW tLongJump.x
LDGW tLongJump.x
INC
STGW tLongJump.x
LDGW tLongJump.x
INC
STGW tLongJump.x
LDGW tLongJump.x
INC
STGW tLongJump.x
!       x := x + 1; x := x + 1; x := x + 1; x := x + 1; x := x + 1; 
LDGW tLongJump.x
INC
STGW tLongJump.x
LDGW tLongJump.x
INC
STGW tLongJump.x
LDGW tLongJump.x
INC
STGW tLongJump.x
LDGW tLongJump.x
INC
STGW tLongJump.x
LDGW tLongJump.x
INC
STGW tLongJump.x
!       x := x + 1; x := x + 1; x := x + 1; x := x + 1; x := x + 1; 
LDGW tLongJump.x
INC
STGW tLongJump.x
LDGW tLongJump.x
INC
STGW tLongJump.x
LDGW tLongJump.x
INC
STGW tLongJump.x
LDGW tLongJump.x
INC
STGW tLongJump.x
LDGW tLongJump.x
INC
STGW tLongJump.x
!       x := x + 1; x := x + 1; x := x + 1; x := x + 1; x := x + 1; 
LDGW tLongJump.x
INC
STGW tLongJump.x
LDGW tLongJump.x
INC
STGW tLongJump.x
LDGW tLongJump.x
INC
STGW tLongJump.x
LDGW tLongJump.x
INC
STGW tLongJump.x
LDGW tLongJump.x
INC
STGW tLongJump.x
!       x := x + 1; x := x + 1; x := x + 1; x := x + 1; x := x + 1; 
LDGW tLongJump.x
INC
STGW tLongJump.x
LDGW tLongJump.x
INC
STGW tLongJump.x
LDGW tLongJump.x
INC
STGW tLongJump.x
LDGW tLongJump.x
INC
STGW tLongJump.x
LDGW tLongJump.x
INC
STGW tLongJump.x
!       x := x + 1; x := x + 1; x := x + 1; x := x + 1; x := x + 1; 
LDGW tLongJump.x
INC
STGW tLongJump.x
LDGW tLongJump.x
INC
STGW tLongJump.x
LDGW tLongJump.x
INC
STGW tLongJump.x
LDGW tLongJump.x
INC
STGW tLongJump.x
LDGW tLongJump.x
INC
STGW tLongJump.x
!       x := x + 1; x := x + 1; x := x + 1; x := x + 1; x := x + 1; 
LDGW tLongJump.x
INC
STGW tLongJump.x
LDGW tLongJump.x
INC
STGW tLongJump.x
LDGW tLongJump.x
INC
STGW tLongJump.x
LDGW tLongJump.x
INC
STGW tLongJump.x
LDGW tLongJump.x
INC
STGW tLongJump.x
!       x := x + 1; x := x + 1; x := x + 1; x := x + 1; x := x + 1; 
LDGW tLongJump.x
INC
STGW tLongJump.x
LDGW tLongJump.x
INC
STGW tLongJump.x
LDGW tLongJump.x
INC
STGW tLongJump.x
LDGW tLongJump.x
INC
STGW tLongJump.x
LDGW tLongJump.x
INC
STGW tLongJump.x
!       x := x + 1; x := x + 1; x := x + 1; x := x + 1; x := x + 1; 
LDGW tLongJump.x
INC
STGW tLongJump.x
LDGW tLongJump.x
INC
STGW tLongJump.x
LDGW tLongJump.x
INC
STGW tLongJump.x
LDGW tLongJump.x
INC
STGW tLongJump.x
LDGW tLongJump.x
INC
STGW tLongJump.x
!       x := x + 1; x := x + 1; x := x + 1; x := x + 1; x := x + 1; 
LDGW tLongJump.x
INC
STGW tLongJump.x
LDGW tLongJump.x
INC
STGW tLongJump.x
LDGW tLongJump.x
INC
STGW tLongJump.x
LDGW tLongJump.x
INC
STGW tLongJump.x
LDGW tLongJump.x
INC
STGW tLongJump.x
!       x := x + 1; x := x + 1; x := x + 1; x := x + 1; x := x + 1; 
LDGW tLongJump.x
INC
STGW tLongJump.x
LDGW tLongJump.x
INC
STGW tLongJump.x
LDGW tLongJump.x
INC
STGW tLongJump.x
LDGW tLongJump.x
INC
STGW tLongJump.x
LDGW tLongJump.x
INC
STGW tLongJump.x
!       x := x + 1; x := x + 1; x := x + 1; x := x + 1; x := x + 1; 
LDGW tLongJump.x
INC
STGW tLongJump.x
LDGW tLongJump.x
INC
STGW tLongJump.x
LDGW tLongJump.x
INC
STGW tLongJump.x
LDGW tLongJump.x
INC
STGW tLongJump.x
LDGW tLongJump.x
INC
STGW tLongJump.x
!       x := x + 1; x := x + 1; x := x + 1; x := x + 1; x := x + 1; 
LDGW tLongJump.x
INC
STGW tLongJump.x
LDGW tLongJump.x
INC
STGW tLongJump.x
LDGW tLongJump.x
INC
STGW tLongJump.x
LDGW tLongJump.x
INC
STGW tLongJump.x
LDGW tLongJump.x
INC
STGW tLongJump.x
!       x := x + 1; x := x + 1; x := x + 1; x := x + 1; x := x + 1; 
LDGW tLongJump.x
INC
STGW tLongJump.x
LDGW tLongJump.x
INC
STGW tLongJump.x
LDGW tLongJump.x
INC
STGW tLongJump.x
LDGW tLongJump.x
INC
STGW tLongJump.x
LDGW tLongJump.x
INC
STGW tLongJump.x
!       x := x + 1; x := x + 1; x := x + 1; x := x + 1; x := x + 1; 
LDGW tLongJump.x
INC
STGW tLongJump.x
LDGW tLongJump.x
INC
STGW tLongJump.x
LDGW tLongJump.x
INC
STGW tLongJump.x
LDGW tLongJump.x
INC
STGW tLongJump.x
LDGW tLongJump.x
INC
STGW tLongJump.x
!       x := x + 1; x := x + 1; x := x + 1; x := x + 1; x := x + 1; 
LDGW tLongJump.x
INC
STGW tLongJump.x
LDGW tLongJump.x
INC
STGW tLongJump.x
LDGW tLongJump.x
INC
STGW tLongJump.x
LDGW tLongJump.x
INC
STGW tLongJump.x
LDGW tLongJump.x
INC
STGW tLongJump.x
!       x := x + 1; x := x + 1; x := x + 1; x := x + 1; x := x + 1; 
LDGW tLongJump.x
INC
STGW tLongJump.x
LDGW tLongJump.x
INC
STGW tLongJump.x
LDGW tLongJump.x
INC
STGW tLongJump.x
LDGW tLongJump.x
INC
STGW tLongJump.x
LDGW tLongJump.x
INC
STGW tLongJump.x
!       x := x + 1; x := x + 1; x := x + 1; x := x + 1; x := x + 1; 
LDGW tLongJump.x
INC
STGW tLongJump.x
LDGW tLongJump.x
INC
STGW tLongJump.x
LDGW tLongJump.x
INC
STGW tLongJump.x
LDGW tLongJump.x
INC
STGW tLongJump.x
LDGW tLongJump.x
INC
STGW tLongJump.x
!       x := x + 1; x := x + 1; x := x + 1; x := x + 1; x := x + 1
LDGW tLongJump.x
INC
STGW tLongJump.x
LDGW tLongJump.x
INC
STGW tLongJump.x
LDGW tLongJump.x
INC
STGW tLongJump.x
LDGW tLongJump.x
INC
STGW tLongJump.x
LDGW tLongJump.x
INC
STGW tLongJump.x
JUMP L4
LABEL L7
!       x := x+50
LDGW tLongJump.x
CONST 50
PLUS
STGW tLongJump.x
JUMP L4
LABEL L3
!       x := x+1
LDGW tLongJump.x
INC
STGW tLongJump.x
LABEL L4
!     Out.Char(' '); Out.Int(x,0)
CONST 32
ALIGNC
GLOBAL Out.Char
CALL 1
CONST 0
LDGW tLongJump.x
GLOBAL Out.Int
CALL 2
!   UNTIL x >= 503;
LDGW tLongJump.x
CONST 503
JLT L1
!   Out.Ln
GLOBAL Out.Ln
CALL 0
RETURN
END

! Global variables
GLOVAR tLongJump.x 4

! End of file
]]*)
