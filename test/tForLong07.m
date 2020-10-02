MODULE tForLong07;
                
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
END tForLong07.

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
!! (SYMFILE #tForLong07 STAMP #tForLong07.%main 1 #tForLong07.m)
!! (CHKSUM STAMP)
!! 
MODULE tForLong07 STAMP 0
IMPORT Out STAMP
ENDHDR

PROC tForLong07.%main 0 4 0
!   FOR i := -2 TO 2 BY 2 DO
CONST -2
CONVNQ
STGQ tForLong07.i
LABEL L1
LDGQ tForLong07.i
CONST 2
CONVNQ
QJGT L2
!     Out.LongInt(i, 0); Out.Ln()
CONST 0
LDGQ tForLong07.i
GLOBAL Out.LongInt
CALL 3
GLOBAL Out.Ln
CALL 0
!   FOR i := -2 TO 2 BY 2 DO
LDGQ tForLong07.i
CONST 2
CONVNQ
QPLUS
STGQ tForLong07.i
JUMP L1
LABEL L2
!   i := -2;
CONST -2
CONVNQ
STGQ tForLong07.i
LABEL L3
!   WHILE i <= 2 DO
LDGQ tForLong07.i
CONST 2
CONVNQ
QJGT L5
!     Out.LongInt(i, 0); Out.Ln;
CONST 0
LDGQ tForLong07.i
GLOBAL Out.LongInt
CALL 3
GLOBAL Out.Ln
CALL 0
!     INC(i, 1)
CONST 1
CONVNQ
GLOBAL tForLong07.i
GLOBAL INCLONG
CALL 3
JUMP L3
LABEL L5
RETURN
END

! Global variables
GLOVAR tForLong07.i 8

! End of file
]]*)
