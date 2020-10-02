MODULE tValReal07;
(* Alex Shiryaev: SYSTEM.VAL can force the JIT to find a float value
   in an integer register, and vice versa *)
IMPORT SYSTEM, Out;
VAR x: REAL; n: INTEGER;
BEGIN
  x := SYSTEM.VAL(REAL, {0}) + 1.0E-45;
  Out.Real(x); Out.Ln;

  x := 3.14; n := SYSTEM.VAL(INTEGER, x + 0.5) + 3;
  Out.Int(n, 0); Out.Ln		
END tValReal07.

(*<<
2.80260E-45
1080620486
>>*)

(*[[
!! (SYMFILE #tValReal07 STAMP #tValReal07.%main 1 #tValReal07.m)
!! (CHKSUM STAMP)
!! 
MODULE tValReal07 STAMP 0
IMPORT Out STAMP
ENDHDR

PROC tValReal07.%main 0 3 0
!   x := SYSTEM.VAL(REAL, {0}) + 1.0E-45;
CONST 1
FCONST 1.0e-45
FPLUS
STGF tValReal07.x
!   Out.Real(x); Out.Ln;
LDGF tValReal07.x
GLOBAL Out.Real
CALL 1
GLOBAL Out.Ln
CALL 0
!   x := 3.14; n := SYSTEM.VAL(INTEGER, x + 0.5) + 3;
FCONST 3.14
STGF tValReal07.x
LDGF tValReal07.x
FCONST 0.5
FPLUS
CONST 3
PLUS
STGW tValReal07.n
!   Out.Int(n, 0); Out.Ln		
CONST 0
LDGW tValReal07.n
GLOBAL Out.Int
CALL 2
GLOBAL Out.Ln
CALL 0
RETURN
END

! Global variables
GLOVAR tValReal07.x 4
GLOVAR tValReal07.n 4

! End of file
]]*)
