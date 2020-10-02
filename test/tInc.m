MODULE tInc;
IMPORT Out;
VAR i: INTEGER;
BEGIN
  i := 10;
  INC(i);
  DEC(i,2);
  INC(i,i);
  Out.Int(i,0);
  Out.Ln
END tInc.

(*<<
18
>>*)

(*[[
!! (SYMFILE #tInc STAMP #tInc.%main 1 #tInc.m)
!! (CHKSUM STAMP)
!! 
MODULE tInc STAMP 0
IMPORT Out STAMP
ENDHDR

PROC tInc.%main 0 3 0
!   i := 10;
CONST 10
STGW tInc.i
!   INC(i);
LDGW tInc.i
INC
STGW tInc.i
!   DEC(i,2);
LDGW tInc.i
CONST 2
MINUS
STGW tInc.i
!   INC(i,i);
GLOBAL tInc.i
DUP 0
LOADW
LDGW tInc.i
PLUS
SWAP
STOREW
!   Out.Int(i,0);
CONST 0
LDGW tInc.i
GLOBAL Out.Int
CALL 2
!   Out.Ln
GLOBAL Out.Ln
CALL 0
RETURN
END

! Global variables
GLOVAR tInc.i 4

! End of file
]]*)
