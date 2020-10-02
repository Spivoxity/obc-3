MODULE tPrimVal07;

IMPORT Out, Conv;

VAR p: PROCEDURE (x: ARRAY OF CHAR): INTEGER;

BEGIN
  p := Conv.IntVal;
  Out.Int(p("24")+p("18"), 0); Out.Ln
END tPrimVal07.

(*<<
42
>>*)

(*[[
!! (SYMFILE #tPrimVal07 STAMP #tPrimVal07.%main 1 #tPrimVal07.m)
!! (CHKSUM STAMP)
!! 
MODULE tPrimVal07 STAMP 0
IMPORT Out STAMP
IMPORT Conv STAMP
ENDHDR

PROC tPrimVal07.%main 0 5 0
!   p := Conv.IntVal;
GLOBAL Conv.IntVal
STGW tPrimVal07.p
!   Out.Int(p("24")+p("18"), 0); Out.Ln
CONST 0
CONST 3
GLOBAL tPrimVal07.%1
LDGW tPrimVal07.p
NCHECK 9
CALLW 2
CONST 3
GLOBAL tPrimVal07.%2
LDGW tPrimVal07.p
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
GLOVAR tPrimVal07.p 4

! String "24"
DEFINE tPrimVal07.%1
STRING 323400

! String "18"
DEFINE tPrimVal07.%2
STRING 313800

! End of file
]]*)

