MODULE tLong;

(*<<
Greater
6.00000000000
1.732051
1.23000000000E+20
>>*)

IMPORT Out, MathL;

PROCEDURE Mult(x, y: LONGREAL): LONGREAL;
BEGIN
  RETURN x * y
END Mult;

VAR x: LONGREAL;

BEGIN
  x := 3.0;
  IF x > 2.0 THEN Out.String("Greater"); Out.Ln END;
  Out.LongReal(Mult(2.0, x)); Out.Ln;
  Out.Fixed(MathL.Sqrt(x), 0, 6); Out.Ln;
  Out.LongReal(1.23D20); Out.Ln
END tLong.

(*[[
!! (SYMFILE #tLong STAMP #tLong.%main 1 #tLong.m)
!! (CHKSUM STAMP)
!! 
MODULE tLong STAMP 0
IMPORT Out STAMP
IMPORT MathL STAMP
ENDHDR

PROC tLong.Mult 0 4 0
! PROCEDURE Mult(x, y: LONGREAL): LONGREAL;
!   RETURN x * y
LDLD 12
LDLD 20
DTIMES
RETURN
END

PROC tLong.%main 0 5 0
!   x := 3.0;
DCONST 3.0
STGD tLong.x
!   IF x > 2.0 THEN Out.String("Greater"); Out.Ln END;
LDGD tLong.x
DCONST 2.0
DJNGT L4
CONST 8
GLOBAL tLong.%1
GLOBAL Out.String
CALL 2
GLOBAL Out.Ln
CALL 0
LABEL L4
!   Out.LongReal(Mult(2.0, x)); Out.Ln;
LDGD tLong.x
DCONST 2.0
GLOBAL tLong.Mult
CALLD 4
GLOBAL Out.LongReal
CALL 2
GLOBAL Out.Ln
CALL 0
!   Out.Fixed(MathL.Sqrt(x), 0, 6); Out.Ln;
CONST 6
CONST 0
LDGD tLong.x
GLOBAL MathL.Sqrt
CALLD 2
GLOBAL Out.Fixed
CALL 4
GLOBAL Out.Ln
CALL 0
!   Out.LongReal(1.23D20); Out.Ln
DCONST 1.23e+20
GLOBAL Out.LongReal
CALL 2
GLOBAL Out.Ln
CALL 0
RETURN
END

! Global variables
GLOVAR tLong.x 8

! String "Greater"
DEFINE tLong.%1
STRING 4772656174657200

! End of file
]]*)
