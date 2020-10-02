MODULE tPParam;

IMPORT Out;

PROCEDURE r;
BEGIN
  Out.String("R"); Out.Ln
END r;

PROCEDURE s;
BEGIN
  Out.String("S"); Out.Ln
END s;

PROCEDURE p(q: PROCEDURE);
BEGIN
  q := s;
  q
END p;

BEGIN
  p(r)
END tPParam.
  
(*<<
S
>>*)

(*[[
!! (SYMFILE #tPParam STAMP #tPParam.%main 1 #tPParam.m)
!! (CHKSUM STAMP)
!! 
MODULE tPParam STAMP 0
IMPORT Out STAMP
ENDHDR

PROC tPParam.r 0 3 0
! PROCEDURE r;
!   Out.String("R"); Out.Ln
CONST 2
GLOBAL tPParam.%1
GLOBAL Out.String
CALL 2
GLOBAL Out.Ln
CALL 0
RETURN
END

PROC tPParam.s 0 3 0
! PROCEDURE s;
!   Out.String("S"); Out.Ln
CONST 2
GLOBAL tPParam.%2
GLOBAL Out.String
CALL 2
GLOBAL Out.Ln
CALL 0
RETURN
END

PROC tPParam.p 0 3 0
! PROCEDURE p(q: PROCEDURE);
!   q := s;
CONST 0
STLW 16
GLOBAL tPParam.s
STLW 12
!   q
LDLW 16
STATLINK
LDLW 12
NCHECK 18
CALL 0
RETURN
END

PROC tPParam.%main 0 3 0
!   p(r)
CONST 0
GLOBAL tPParam.r
GLOBAL tPParam.p
CALL 2
RETURN
END

! String "R"
DEFINE tPParam.%1
STRING 5200

! String "S"
DEFINE tPParam.%2
STRING 5300

! End of file
]]*)
