MODULE tProcVarParam;

(* VAR parameter of procedure type *)

IMPORT Conv, Out;

TYPE Cvt = PROCEDURE (CONST s: ARRAY OF CHAR): INTEGER;

PROCEDURE Choose(VAR p: Cvt);
BEGIN
  p := Conv.IntVal
END Choose;

VAR p: Cvt;

BEGIN
  Choose(p);
  Out.Int(p("1234"), 0); Out.Ln
END tProcVarParam.

(*<<
1234
>>*)

(*[[
!! (SYMFILE #tProcVarParam STAMP #tProcVarParam.%main 1 #tProcVarParam.m)
!! (CHKSUM STAMP)
!! 
MODULE tProcVarParam STAMP 0
IMPORT Conv STAMP
IMPORT Out STAMP
ENDHDR

PROC tProcVarParam.Choose 0 2 0x00100001
! PROCEDURE Choose(VAR p: Cvt);
!   p := Conv.IntVal
GLOBAL Conv.IntVal
LDLW 12
STOREW
RETURN
END

PROC tProcVarParam.%main 0 4 0
!   Choose(p);
GLOBAL tProcVarParam.p
GLOBAL tProcVarParam.Choose
CALL 1
!   Out.Int(p("1234"), 0); Out.Ln
CONST 0
CONST 5
GLOBAL tProcVarParam.%1
LDGW tProcVarParam.p
NCHECK 18
CALLW 2
GLOBAL Out.Int
CALL 2
GLOBAL Out.Ln
CALL 0
RETURN
END

! Global variables
GLOVAR tProcVarParam.p 4

! String "1234"
DEFINE tProcVarParam.%1
STRING 3132333400

! End of file
]]*)
