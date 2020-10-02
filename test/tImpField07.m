MODULE tImpField07;
IMPORT t := xTypes07, Out;
VAR s: t.rec;
BEGIN
  s.x := 3;
  Out.Int(s.x, 0); Out.Ln
END tImpField07.

(*<<
3
>>*)

(*[[
!! (SYMFILE #tImpField07 STAMP #tImpField07.%main 1 #tImpField07.m)
!! (CHKSUM STAMP)
!! 
MODULE tImpField07 STAMP 0
IMPORT xTypes07 STAMP
IMPORT Out STAMP
ENDHDR

PROC tImpField07.%main 0 3 0
!   s.x := 3;
CONST 3
GLOBAL tImpField07.s
STNW 4
!   Out.Int(s.x, 0); Out.Ln
CONST 0
GLOBAL tImpField07.s
LDNW 4
GLOBAL Out.Int
CALL 2
GLOBAL Out.Ln
CALL 0
RETURN
END

! Global variables
GLOVAR tImpField07.s 8

! End of file
]]*)
