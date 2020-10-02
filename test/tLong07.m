MODULE tLong07;

(*<<
Greater
6.00000000000
1.732051
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
  Out.Fixed(MathL.Sqrt(x), 0, 6); Out.Ln
END tLong07.

(*[[
!! (SYMFILE #tLong07 STAMP #tLong07.%main 1 #tLong07.m)
!! (CHKSUM STAMP)
!! 
MODULE tLong07 STAMP 0
IMPORT Out STAMP
IMPORT MathL STAMP
ENDHDR

PROC tLong07.Mult 0 4 0
! PROCEDURE Mult(x, y: LONGREAL): LONGREAL;
!   RETURN x * y
LDLD 12
LDLD 20
DTIMES
RETURN
END

PROC tLong07.%main 0 5 0
!   x := 3.0;
DCONST 3.0
STGD tLong07.x
!   IF x > 2.0 THEN Out.String("Greater"); Out.Ln END;
LDGD tLong07.x
DCONST 2.0
DJNGT L4
CONST 8
GLOBAL tLong07.%1
GLOBAL Out.String
CALL 2
GLOBAL Out.Ln
CALL 0
LABEL L4
!   Out.LongReal(Mult(2.0, x)); Out.Ln;
LDGD tLong07.x
DCONST 2.0
GLOBAL tLong07.Mult
CALLD 4
GLOBAL Out.LongReal
CALL 2
GLOBAL Out.Ln
CALL 0
!   Out.Fixed(MathL.Sqrt(x), 0, 6); Out.Ln
CONST 6
CONST 0
LDGD tLong07.x
GLOBAL MathL.Sqrt
CALLD 2
GLOBAL Out.Fixed
CALL 4
GLOBAL Out.Ln
CALL 0
RETURN
END

! Global variables
GLOVAR tLong07.x 8

! String "Greater"
DEFINE tLong07.%1
STRING 4772656174657200

! End of file
]]*)
