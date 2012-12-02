MODULE tPrimVal;

IMPORT Out, Conv;

VAR p: PROCEDURE (x: ARRAY OF CHAR): INTEGER;

BEGIN
  p := Conv.IntVal;
  Out.Int(p("24")+p("18"), 0); Out.Ln
END tPrimVal.

(*<<
42
>>*)

(*[[
!! SYMFILE #tPrimVal STAMP #tPrimVal.%main 1
!! END STAMP
!! 
MODULE tPrimVal STAMP 0
IMPORT Out STAMP
IMPORT Conv STAMP
ENDHDR

PROC tPrimVal.%main 0 20 0
!   p := Conv.IntVal;
CONST Conv.IntVal
STGW tPrimVal.p
!   Out.Int(p("24")+p("18"), 0); Out.Ln
CONST 0
CONST 3
CONST tPrimVal.%1
LDGW tPrimVal.p
NCHECK 9
CALLW 2
CONST 3
CONST tPrimVal.%2
LDGW tPrimVal.p
NCHECK 9
CALLW 2
PLUS
CONST Out.Int
CALL 2
CONST Out.Ln
CALL 0
RETURN
END

! Global variables
GLOBAL tPrimVal.p 4

! String "24"
DEFINE tPrimVal.%1
STRING 323400

! String "18"
DEFINE tPrimVal.%2
STRING 313800

! End of file
]]*)

