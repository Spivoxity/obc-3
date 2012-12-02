MODULE tForLong;
                
IMPORT Out;
                
VAR
  i: LONGINT;
                
BEGIN
  FOR i := -2 TO 2 BY 2 DO
    Out.LongInt(i, 0); Out.Ln()
  END;

  i := -2;
  WHILE i <= 2 DO
    Out.LongInt(i, 0); Out.Ln;
    INC(i, 1)
  END
END tForLong.

(*<<
-2
0
2
-2
-1
0
1
2
>>*)

(*[[
!! SYMFILE #tForLong STAMP #tForLong.%main 1
!! END STAMP
!! 
MODULE tForLong STAMP 0
IMPORT Out STAMP
ENDHDR

PROC tForLong.%main 0 32 0
!   FOR i := -2 TO 2 BY 2 DO
CONST -2
CONVNQ
STGQ tForLong.i
JUMP 2
LABEL 1
!     Out.LongInt(i, 0); Out.Ln()
CONST 0
LDGQ tForLong.i
CONST Out.LongInt
CALL 3
CONST Out.Ln
CALL 0
!   FOR i := -2 TO 2 BY 2 DO
LDGQ tForLong.i
CONST 2
CONVNQ
QPLUS
STGQ tForLong.i
LABEL 2
LDGQ tForLong.i
CONST 2
CONVNQ
QJLEQ 1
!   i := -2;
CONST -2
CONVNQ
STGQ tForLong.i
JUMP 4
LABEL 3
!     Out.LongInt(i, 0); Out.Ln;
CONST 0
LDGQ tForLong.i
CONST Out.LongInt
CALL 3
CONST Out.Ln
CALL 0
!     INC(i, 1)
CONST 1
CONVNQ
CONST tForLong.i
CONST INCLONG
CALLQ 3
LABEL 4
!   WHILE i <= 2 DO
LDGQ tForLong.i
CONST 2
CONVNQ
QJLEQ 3
RETURN
END

! Global variables
GLOBAL tForLong.i 8

! End of file
]]*)
