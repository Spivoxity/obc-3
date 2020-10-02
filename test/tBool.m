MODULE tBool;

(*<<
5
>>*)

IMPORT Out;

VAR x: REAL; b: BOOLEAN;

BEGIN
  x := 3;
  b := ~ (x > 4);
  IF b THEN x := 5 END;
  Out.Fixed(x, 0, 0); Out.Ln
END tBool.

(*[[
!! (SYMFILE #tBool STAMP #tBool.%main 1 #tBool.m)
!! (CHKSUM STAMP)
!! 
MODULE tBool STAMP 0
IMPORT Out STAMP
ENDHDR

PROC tBool.%main 0 5 0
!   x := 3;
FCONST 3.0
STGF tBool.x
!   b := ~ (x > 4);
LDGF tBool.x
FCONST 4.0
FGT
NOT
STGC tBool.b
!   IF b THEN x := 5 END;
LDGC tBool.b
JEQZ L3
FCONST 5.0
STGF tBool.x
LABEL L3
!   Out.Fixed(x, 0, 0); Out.Ln
CONST 0
CONST 0
LDGF tBool.x
CONVFD
GLOBAL Out.Fixed
CALL 4
GLOBAL Out.Ln
CALL 0
RETURN
END

! Global variables
GLOVAR tBool.x 4
GLOVAR tBool.b 1

! End of file
]]*)
