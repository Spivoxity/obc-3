MODULE tPi;

(*<<
3.14159265359
3.14159265359
>>*)

IMPORT MathL, Out;

PROCEDURE arctan(arg: LONGREAL): LONGREAL;
  VAR pow, term, sum: LONGREAL; n: INTEGER;
BEGIN
  n := 1; pow := arg; sum := arg;
  REPEAT
    n := n+2;
    pow := -pow*arg*arg;
    term := pow/n;
    sum := sum+term
  UNTIL ABS(term) < 1.0E-12;
  RETURN sum
END arctan;

BEGIN
  Out.LongReal(16*arctan(1/5)-4*arctan(1/239)); Out.Ln;
  Out.LongReal(MathL.pi); Out.Ln
END tPi.

(*[[
!! (SYMFILE #tPi STAMP #tPi.%main 1)
!! (CHKSUM STAMP)
!! 
MODULE tPi STAMP 0
IMPORT MathL STAMP
IMPORT Out STAMP
ENDHDR

PROC tPi.%main 0 3 0
!   Out.LongReal(MathL.pi); Out.Ln
DCONST 3.14159265359
GLOBAL Out.LongReal
CALL 2
GLOBAL Out.Ln
CALL 0
RETURN
END

! End of file
]]*)
