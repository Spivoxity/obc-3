MODULE tComp07;

(*<<
1
>>*)

IMPORT Out;

VAR x, y: REAL; b: BOOLEAN;

BEGIN
  x := FLT(3); y := FLT(4);
  b := (x < FLT(4));
  Out.Int(ORD(b), 0); Out.Ln
END tComp07.

(*[[
!! (SYMFILE #tComp07 STAMP #tComp07.%main 1 #tComp07.m)
!! (CHKSUM STAMP)
!! 
MODULE tComp07 STAMP 0
IMPORT Out STAMP
ENDHDR

PROC tComp07.%main 0 3 0
!   x := FLT(3); y := FLT(4);
FCONST 3.0
STGF tComp07.x
FCONST 4.0
STGF tComp07.y
!   b := (x < FLT(4));
LDGF tComp07.x
FCONST 4.0
FLT
STGC tComp07.b
!   Out.Int(ORD(b), 0); Out.Ln
CONST 0
LDGC tComp07.b
GLOBAL Out.Int
CALL 2
GLOBAL Out.Ln
CALL 0
RETURN
END

! Global variables
GLOVAR tComp07.x 4
GLOVAR tComp07.y 4
GLOVAR tComp07.b 1

! End of file
]]*)
