MODULE tInc07;
IMPORT Out;
VAR i: INTEGER;
BEGIN
  i := 10;
  INC(i);
  DEC(i,2);
  INC(i,i);
  Out.Int(i,0);
  Out.Ln
END tInc07.

(*<<
18
>>*)

(*[[
!! (SYMFILE #tInc07 STAMP #tInc07.%main 1 #tInc07.m)
!! (CHKSUM STAMP)
!! 
MODULE tInc07 STAMP 0
IMPORT Out STAMP
ENDHDR

PROC tInc07.%main 0 3 0
!   i := 10;
CONST 10
STGW tInc07.i
!   INC(i);
LDGW tInc07.i
INC
STGW tInc07.i
!   DEC(i,2);
LDGW tInc07.i
CONST 2
MINUS
STGW tInc07.i
!   INC(i,i);
GLOBAL tInc07.i
DUP 0
LOADW
LDGW tInc07.i
PLUS
SWAP
STOREW
!   Out.Int(i,0);
CONST 0
LDGW tInc07.i
GLOBAL Out.Int
CALL 2
!   Out.Ln
GLOBAL Out.Ln
CALL 0
RETURN
END

! Global variables
GLOVAR tInc07.i 4

! End of file
]]*)
