MODULE tPi07;

(*<<
3.14159265359
>>*)

IMPORT MathL, Out;

BEGIN
  Out.LongReal(MathL.pi, 0); Out.Ln
END tPi07.

(*[[
!! (SYMFILE #tPi07 STAMP #tPi07.%main 1 #tPi07.m)
!! (CHKSUM STAMP)
!! 
MODULE tPi07 STAMP 0
IMPORT MathL STAMP
IMPORT Out STAMP
ENDHDR

PROC tPi07.%main 0 4 0
!   Out.LongReal(MathL.pi, 0); Out.Ln
CONST 0
DCONST 3.14159265359
GLOBAL Out.LongReal
CALL 3
GLOBAL Out.Ln
CALL 0
RETURN
END

! End of file
]]*)
