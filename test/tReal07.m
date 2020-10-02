MODULE tReal07;

(*<<
-3.1415927410
-1.57080
yes
>>*)

IMPORT Out, Math;

VAR r: REAL;

BEGIN
  r := Math.pi;
  Out.Fixed(-r, 0, 10); Out.Ln;
  Out.Real(Math.Arctan2(-1.0,0.0)); Out.Ln;
  IF 2.0 + r > 5.14 THEN
     Out.String("yes"); Out.Ln
  END
END tReal07.

(*[[
!! (SYMFILE #tReal07 STAMP #tReal07.%main 1 #tReal07.m)
!! (CHKSUM STAMP)
!! 
MODULE tReal07 STAMP 0
IMPORT Out STAMP
IMPORT Math STAMP
ENDHDR

PROC tReal07.%main 0 5 0
!   r := Math.pi;
FCONST 3.14159265359
STGF tReal07.r
!   Out.Fixed(-r, 0, 10); Out.Ln;
CONST 10
CONST 0
LDGF tReal07.r
FUMINUS
CONVFD
GLOBAL Out.Fixed
CALL 4
GLOBAL Out.Ln
CALL 0
!   Out.Real(Math.Arctan2(-1.0,0.0)); Out.Ln;
FCONST 0.0
FCONST -1.0
GLOBAL Math.Arctan2
CALLF 2
GLOBAL Out.Real
CALL 1
GLOBAL Out.Ln
CALL 0
!   IF 2.0 + r > 5.14 THEN
LDGF tReal07.r
FCONST 2.0
FPLUS
FCONST 5.14
FJNGT L4
!      Out.String("yes"); Out.Ln
CONST 4
GLOBAL tReal07.%1
GLOBAL Out.String
CALL 2
GLOBAL Out.Ln
CALL 0
LABEL L4
RETURN
END

! Global variables
GLOVAR tReal07.r 4

! String "yes"
DEFINE tReal07.%1
STRING 79657300

! End of file
]]*)
