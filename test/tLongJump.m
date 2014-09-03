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
!! SYMFILE #tLongJump STAMP #tLongJump.%main 1
!! END STAMP
!! 
MODULE tLongJump STAMP 0
IMPORT Out STAMP
ENDHDR

PROC tLongJump.%main 0 3 0
!   x := 0;
CONST 0
STGW tLongJump.x
LABEL 1
!     CASE x OF
LDGW tLongJump.x
CONST 502
TESTGEQ 8
CONST 301
TESTGEQ 9
CONST 1
JGEQ 6
JUMP 3
LABEL 9
CONST 499
JLEQ 7
JUMP 3
LABEL 8
CONST 502
JNEQ 3
!       x := x+3
LDGW tLongJump.x
CONST 3
PLUS
STGW tLongJump.x
JUMP 4
LABEL 6
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
JUMP 4
LABEL 7
!       x := x+50
LDGW tLongJump.x
CONST 50
PLUS
STGW tLongJump.x
JUMP 4
LABEL 3
!       x := x+1
LDGW tLongJump.x
INC
STGW tLongJump.x
LABEL 4
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
JLT 1
!   Out.Ln
GLOBAL Out.Ln
CALL 0
RETURN
END

! Global variables
GLOVAR tLongJump.x 4

! End of file
]]*)
