MODULE tBool07;

(*<<
5
>>*)

IMPORT Out;

VAR x: REAL; b: BOOLEAN;

BEGIN
  x := FLT(3);
  b := ~ (x > FLT(4));
  IF b THEN x := FLT(5) END;
  Out.Fixed(x, 0, 0); Out.Ln
END tBool07.

(*[[
!! (SYMFILE #tBool07 STAMP #tBool07.%main 1 #tBool07.m)
!! (CHKSUM STAMP)
!! 
MODULE tBool07 STAMP 0
IMPORT Out STAMP
ENDHDR

PROC tBool07.%main 0 5 0
!   x := FLT(3);
FCONST 3.0
STGF tBool07.x
!   b := ~ (x > FLT(4));
LDGF tBool07.x
FCONST 4.0
FGT
NOT
STGC tBool07.b
!   IF b THEN x := FLT(5) END;
LDGC tBool07.b
JEQZ L3
FCONST 5.0
STGF tBool07.x
LABEL L3
!   Out.Fixed(x, 0, 0); Out.Ln
CONST 0
CONST 0
LDGF tBool07.x
CONVFD
GLOBAL Out.Fixed
CALL 4
GLOBAL Out.Ln
CALL 0
RETURN
END

! Global variables
GLOVAR tBool07.x 4
GLOVAR tBool07.b 1

! End of file
]]*)
