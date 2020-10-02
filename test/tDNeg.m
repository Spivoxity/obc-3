MODULE tDNeg;

IMPORT Math, Out;

VAR x: LONGREAL;

BEGIN
  x := Math.pi;
  x := -x;
  Out.LongReal(x); Out.Ln
END tDNeg.

(*<<
-3.14159265359
>>*)

(*[[
!! (SYMFILE #tDNeg STAMP #tDNeg.%main 1 #tDNeg.m)
!! (CHKSUM STAMP)
!! 
MODULE tDNeg STAMP 0
IMPORT Math STAMP
IMPORT Out STAMP
ENDHDR

PROC tDNeg.%main 0 3 0
!   x := Math.pi;
DCONST 3.14159265359
STGD tDNeg.x
!   x := -x;
LDGD tDNeg.x
DUMINUS
STGD tDNeg.x
!   Out.LongReal(x); Out.Ln
LDGD tDNeg.x
GLOBAL Out.LongReal
CALL 2
GLOBAL Out.Ln
CALL 0
RETURN
END

! Global variables
GLOVAR tDNeg.x 8

! End of file
]]*)
