MODULE tBigReal;

(*<<
2.00000E+32
2.20000E-20
>>*)
IMPORT Out;
BEGIN
  Out.Real(2.0E+32); Out.Ln;
  Out.Real(2.2E-20); Out.Ln
END tBigReal.

(*[[
!! (SYMFILE #tBigReal STAMP #tBigReal.%main 1 #tBigReal.m)
!! (CHKSUM STAMP)
!! 
MODULE tBigReal STAMP 0
IMPORT Out STAMP
ENDHDR

PROC tBigReal.%main 0 2 0
!   Out.Real(2.0E+32); Out.Ln;
FCONST 2.0e+32
GLOBAL Out.Real
CALL 1
GLOBAL Out.Ln
CALL 0
!   Out.Real(2.2E-20); Out.Ln
FCONST 2.2e-20
GLOBAL Out.Real
CALL 1
GLOBAL Out.Ln
CALL 0
RETURN
END

! End of file
]]*)
