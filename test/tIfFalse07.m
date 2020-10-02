MODULE tIfFalse07;

(*<<
Testing:
>>*)

IMPORT Out;

BEGIN
  Out.String("Testing:");
  IF FALSE THEN Out.String(" Failed") END; 
  Out.Ln
END tIfFalse07.

(*[[
!! (SYMFILE #tIfFalse07 STAMP #tIfFalse07.%main 1 #tIfFalse07.m)
!! (CHKSUM STAMP)
!! 
MODULE tIfFalse07 STAMP 0
IMPORT Out STAMP
ENDHDR

PROC tIfFalse07.%main 0 3 0
!   Out.String("Testing:");
CONST 9
GLOBAL tIfFalse07.%1
GLOBAL Out.String
CALL 2
!   Out.Ln
GLOBAL Out.Ln
CALL 0
RETURN
END

! String "Testing:"
DEFINE tIfFalse07.%1
STRING 54657374696E673A00

! String " Failed"
DEFINE tIfFalse07.%2
STRING 204661696C656400

! End of file
]]*)
