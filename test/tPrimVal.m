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
!! (SYMFILE #tPrimVal STAMP #tPrimVal.%main 1 #tPrimVal.m)
!! (CHKSUM STAMP)
!! 
MODULE tPrimVal STAMP 0
IMPORT Out STAMP
IMPORT Conv STAMP
ENDHDR

PROC tPrimVal.%main 0 5 0
!   p := Conv.IntVal;
GLOBAL Conv.IntVal
STGW tPrimVal.p
!   Out.Int(p("24")+p("18"), 0); Out.Ln
CONST 0
CONST 3
GLOBAL tPrimVal.%1
LDGW tPrimVal.p
NCHECK 9
CALLW 2
CONST 3
GLOBAL tPrimVal.%2
LDGW tPrimVal.p
NCHECK 9
CALLW 2
PLUS
GLOBAL Out.Int
CALL 2
GLOBAL Out.Ln
CALL 0
RETURN
END

! Global variables
GLOVAR tPrimVal.p 4

! String "24"
DEFINE tPrimVal.%1
STRING 323400

! String "18"
DEFINE tPrimVal.%2
STRING 313800

! End of file
]]*)

