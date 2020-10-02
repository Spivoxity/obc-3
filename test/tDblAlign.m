MODULE tDblAlign;

IMPORT MathL, Out;

PROCEDURE P;
  VAR
    x: INTEGER;
    y: LONGREAL;
    z: INTEGER;
    w: LONGREAL;
BEGIN
  y := MathL.pi;
  w := MathL.pi;
  x := 3; z := 4;
  Out.LongReal(y); Out.Ln;
  Out.LongReal(w); Out.Ln
END P;

BEGIN
  P
END tDblAlign.

(*<<
3.14159265359
3.14159265359
>>*)

(*[[
!! (SYMFILE #tDblAlign STAMP #tDblAlign.%main 1 #tDblAlign.m)
!! (CHKSUM STAMP)
!! 
MODULE tDblAlign STAMP 0
IMPORT MathL STAMP
IMPORT Out STAMP
ENDHDR

PROC tDblAlign.P 24 3 0
! PROCEDURE P;
!   y := MathL.pi;
DCONST 3.14159265359
STLD -12
!   w := MathL.pi;
DCONST 3.14159265359
STLD -24
!   x := 3; z := 4;
CONST 3
STLW -4
CONST 4
STLW -16
!   Out.LongReal(y); Out.Ln;
LDLD -12
GLOBAL Out.LongReal
CALL 2
GLOBAL Out.Ln
CALL 0
!   Out.LongReal(w); Out.Ln
LDLD -24
GLOBAL Out.LongReal
CALL 2
GLOBAL Out.Ln
CALL 0
RETURN
END

PROC tDblAlign.%main 0 1 0
!   P
GLOBAL tDblAlign.P
CALL 0
RETURN
END

! End of file
]]*)
