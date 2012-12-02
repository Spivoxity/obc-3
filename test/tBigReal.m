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
!! SYMFILE #tBigReal STAMP #tBigReal.%main 1
!! END STAMP
!! 
MODULE tBigReal STAMP 0
IMPORT Out STAMP
ENDHDR

PROC tBigReal.%main 0 8 0
!   Out.Real(2.0E+32); Out.Ln;
FCONST 2e+32
CONST Out.Real
CALL 1
CONST Out.Ln
CALL 0
!   Out.Real(2.2E-20); Out.Ln
FCONST 2.2e-20
CONST Out.Real
CALL 1
CONST Out.Ln
CALL 0
RETURN
END

! End of file
]]*)
