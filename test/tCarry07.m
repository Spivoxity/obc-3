MODULE tCarry07;

IMPORT Out;

CONST M = 7FFFFFFFH; X = 2 * M + 1;

PROCEDURE Inc(n: LONGINT): LONGINT;
BEGIN
  RETURN 1-n
END Inc;

BEGIN
  Out.LongInt(X, 0); Out.Ln;
  Out.LongInt(Inc(X), 0); Out.Ln
END tCarry07.

(*<<
4294967295
-4294967294
>>*)

(*[[
!! (SYMFILE #tCarry07 STAMP #tCarry07.%main 1)
!! (CHKSUM STAMP)
!! 
MODULE tCarry07 STAMP 0
IMPORT Out STAMP
ENDHDR

PROC tCarry07.Inc 0 4 0
! PROCEDURE Inc(n: LONGINT): LONGINT;
!   RETURN 1-n
CONST 1
CONVNQ
LDLQ 12
QMINUS
RETURNQ
END

PROC tCarry07.%main 0 5 0
!   Out.LongInt(X, 0); Out.Ln;
CONST 0
QCONST 4294967295
GLOBAL Out.LongInt
CALL 3
GLOBAL Out.Ln
CALL 0
!   Out.LongInt(Inc(X), 0); Out.Ln
CONST 0
QCONST 4294967295
GLOBAL tCarry07.Inc
CALLQ 2
GLOBAL Out.LongInt
CALL 3
GLOBAL Out.Ln
CALL 0
RETURN
END

! End of file
]]*)
