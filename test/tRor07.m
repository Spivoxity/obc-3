MODULE tRor07;

IMPORT Out;

VAR x, y: INTEGER;

BEGIN
  x := 5;
  x := ROR(x, 2);
  Out.Int(x, 0); Out.Ln;

  x := 5; y := 2;
  x := ROR(x, y);
  Out.Int(x, 0); Out.Ln;

  x := ROR(5, 2);
  Out.Int(x, 0); Out.Ln;

  y := 1;
  y := LSL(y, 30) + 1;
  Out.Int(y, 0); Out.Ln
END tRor07.

(*<<
1073741825
1073741825
1073741825
1073741825
>>*)

(*[[
!! (SYMFILE #tRor07 STAMP #tRor07.%main 1 #tRor07.m)
!! (CHKSUM STAMP)
!! 
MODULE tRor07 STAMP 0
IMPORT Out STAMP
ENDHDR

PROC tRor07.%main 0 3 0
!   x := 5;
CONST 5
STGW tRor07.x
!   x := ROR(x, 2);
LDGW tRor07.x
CONST 2
ROR
STGW tRor07.x
!   Out.Int(x, 0); Out.Ln;
CONST 0
LDGW tRor07.x
GLOBAL Out.Int
CALL 2
GLOBAL Out.Ln
CALL 0
!   x := 5; y := 2;
CONST 5
STGW tRor07.x
CONST 2
STGW tRor07.y
!   x := ROR(x, y);
LDGW tRor07.x
LDGW tRor07.y
ROR
STGW tRor07.x
!   Out.Int(x, 0); Out.Ln;
CONST 0
LDGW tRor07.x
GLOBAL Out.Int
CALL 2
GLOBAL Out.Ln
CALL 0
!   x := ROR(5, 2);
CONST 1073741825
STGW tRor07.x
!   Out.Int(x, 0); Out.Ln;
CONST 0
LDGW tRor07.x
GLOBAL Out.Int
CALL 2
GLOBAL Out.Ln
CALL 0
!   y := 1;
CONST 1
STGW tRor07.y
!   y := LSL(y, 30) + 1;
LDGW tRor07.y
CONST 30
LSL
INC
STGW tRor07.y
!   Out.Int(y, 0); Out.Ln
CONST 0
LDGW tRor07.y
GLOBAL Out.Int
CALL 2
GLOBAL Out.Ln
CALL 0
RETURN
END

! Global variables
GLOVAR tRor07.x 4
GLOVAR tRor07.y 4

! End of file
]]*)
