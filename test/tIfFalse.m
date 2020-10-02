MODULE tIfFalse;

(*<<
Testing:
>>*)

IMPORT Out;

BEGIN
  Out.String("Testing:");
  IF FALSE THEN Out.String(" Failed") END; 
  Out.Ln
END tIfFalse.

(*[[
!! (SYMFILE #tIfFalse STAMP #tIfFalse.%main 1 #tIfFalse.m)
!! (CHKSUM STAMP)
!! 
MODULE tIfFalse STAMP 0
IMPORT Out STAMP
ENDHDR

PROC tIfFalse.%main 0 3 0
!   Out.String("Testing:");
CONST 9
GLOBAL tIfFalse.%1
GLOBAL Out.String
CALL 2
!   Out.Ln
GLOBAL Out.Ln
CALL 0
RETURN
END

! String "Testing:"
DEFINE tIfFalse.%1
STRING 54657374696E673A00

! String " Failed"
DEFINE tIfFalse.%2
STRING 204661696C656400

! End of file
]]*)
