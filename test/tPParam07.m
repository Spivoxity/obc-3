MODULE tPParam07;

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
END tPParam07.
  
(*<<
S
>>*)

(*[[
!! (SYMFILE #tPParam07 STAMP #tPParam07.%main 1 #tPParam07.m)
!! (CHKSUM STAMP)
!! 
MODULE tPParam07 STAMP 0
IMPORT Out STAMP
ENDHDR

PROC tPParam07.r 0 3 0
! PROCEDURE r;
!   Out.String("R"); Out.Ln
CONST 2
GLOBAL tPParam07.%1
GLOBAL Out.String
CALL 2
GLOBAL Out.Ln
CALL 0
RETURN
END

PROC tPParam07.s 0 3 0
! PROCEDURE s;
!   Out.String("S"); Out.Ln
CONST 2
GLOBAL tPParam07.%2
GLOBAL Out.String
CALL 2
GLOBAL Out.Ln
CALL 0
RETURN
END

PROC tPParam07.p 0 3 0
! PROCEDURE p(q: PROCEDURE);
!   q := s;
CONST 0
STLW 16
GLOBAL tPParam07.s
STLW 12
!   q
LDLW 16
STATLINK
LDLW 12
NCHECK 18
CALL 0
RETURN
END

PROC tPParam07.%main 0 3 0
!   p(r)
CONST 0
GLOBAL tPParam07.r
GLOBAL tPParam07.p
CALL 2
RETURN
END

! String "R"
DEFINE tPParam07.%1
STRING 5200

! String "S"
DEFINE tPParam07.%2
STRING 5300

! End of file
]]*)
