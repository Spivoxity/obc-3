MODULE tSysVal;

IMPORT SYSTEM, Out;

VAR
  byte : SYSTEM.BYTE;
  int : INTEGER;

BEGIN
  byte := SYSTEM.VAL(SYSTEM.BYTE, 15);
  int := SYSTEM.VAL(INTEGER, byte);
  Out.Int(int, 0); Out.Ln
END tSysVal.

(*<<
15
>>*)

(*[[
!! (SYMFILE #tSysVal STAMP #tSysVal.%main 1 #tSysVal.m)
!! (CHKSUM STAMP)
!! 
MODULE tSysVal STAMP 0
IMPORT Out STAMP
ENDHDR

PROC tSysVal.%main 0 3 0
!   byte := SYSTEM.VAL(SYSTEM.BYTE, 15);
CONST 15
STGC tSysVal.byte
!   int := SYSTEM.VAL(INTEGER, byte);
LDGC tSysVal.byte
STGW tSysVal.int
!   Out.Int(int, 0); Out.Ln
CONST 0
LDGW tSysVal.int
GLOBAL Out.Int
CALL 2
GLOBAL Out.Ln
CALL 0
RETURN
END

! Global variables
GLOVAR tSysVal.byte 1
GLOVAR tSysVal.int 4

! End of file
]]*)
