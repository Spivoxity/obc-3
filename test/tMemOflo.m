MODULE tMemOflo;

IMPORT Out;

VAR string*: ARRAY 32*1024*1024 OF CHAR;

BEGIN
  Out.String("OK"); Out.Ln
END tMemOflo.

(*<<
OK
>>*)

(*[[
!! (SYMFILE #tMemOflo STAMP #tMemOflo.%main 1 #tMemOflo.m)
!! (GLOBAL #string* #tMemOflo.string !1 (ARRAY 33554432 CHAR))
!! (CHKSUM STAMP)
!! 
MODULE tMemOflo STAMP 0
IMPORT Out STAMP
ENDHDR

PROC tMemOflo.%main 0 3 0
!   Out.String("OK"); Out.Ln
CONST 3
GLOBAL tMemOflo.%1
GLOBAL Out.String
CALL 2
GLOBAL Out.Ln
CALL 0
RETURN
END

! Global variables
GLOVAR tMemOflo.string 33554432

! String "OK"
DEFINE tMemOflo.%1
STRING 4F4B00

! End of file
]]*)
