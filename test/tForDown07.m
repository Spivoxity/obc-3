MODULE tForDown07;

(*<<
10
9
8
7
6
5
4
3
2
1
>>*)

IMPORT Out;

VAR i: SHORTINT;

BEGIN
  FOR i := 10 TO 1 BY -1 DO
    Out.Int(i, 0); Out.Ln
  END
END tForDown07.

(*[[
!! SYMFILE #tForDown07 STAMP #tForDown07.%main 1
!! END STAMP
!! 
MODULE tForDown07 STAMP 0
IMPORT Out STAMP
ENDHDR

PROC tForDown07.%main 0 3 0
!   FOR i := 10 TO 1 BY -1 DO
CONST 10
STGS tForDown07.i
LABEL 1
LDGS tForDown07.i
CONST 1
JLT 2
!     Out.Int(i, 0); Out.Ln
CONST 0
LDGS tForDown07.i
GLOBAL Out.Int
CALL 2
GLOBAL Out.Ln
CALL 0
LDGS tForDown07.i
DEC
STGS tForDown07.i
JUMP 1
LABEL 2
RETURN
END

! Global variables
GLOVAR tForDown07.i 2

! End of file
]]*)
