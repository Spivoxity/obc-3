MODULE tPi07;

(*<<
3.14159265359
>>*)

IMPORT MathL, Out;

BEGIN
  Out.LongReal(MathL.pi); Out.Ln
END tPi07.

(*[[
!! (SYMFILE #tPi07 STAMP #tPi07.%main 1)
!! (CHKSUM STAMP)
!! 
MODULE tPi07 STAMP 0
IMPORT MathL STAMP
IMPORT Out STAMP
ENDHDR

PROC tPi07.%main 0 4 0
!   Out.LongReal(MathL.pi); Out.Ln
DCONST 3.14159265359
GLOBAL Out.LongReal
CALL 2
GLOBAL Out.Ln
CALL 0
RETURN
END

! End of file
]]*)
