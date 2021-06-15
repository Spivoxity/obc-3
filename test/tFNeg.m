MODULE tFNeg;

(*<<
-8.50940
>>*)

IMPORT Out;

VAR f: REAL;

BEGIN
  f := 2.71;
  f := 3.14 * (-f);
  Out.Real(f, 0); Out.Ln
END tFNeg.

(*[[
!! (SYMFILE #tFNeg STAMP #tFNeg.%main 1 #tFNeg.m)
!! (CHKSUM STAMP)
!! 
MODULE tFNeg STAMP 0
IMPORT Out STAMP
ENDHDR

PROC tFNeg.%main 0 3 0
!   f := 2.71;
FCONST 2.71
STGF tFNeg.f
!   f := 3.14 * (-f);
LDGF tFNeg.f
FUMINUS
FCONST 3.14
FTIMES
STGF tFNeg.f
!   Out.Real(f, 0); Out.Ln
CONST 0
LDGF tFNeg.f
GLOBAL Out.Real
CALL 2
GLOBAL Out.Ln
CALL 0
RETURN
END

! Global variables
GLOVAR tFNeg.f 4

! End of file
]]*)
