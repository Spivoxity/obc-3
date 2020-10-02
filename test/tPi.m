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
!! (SYMFILE #tPi STAMP #tPi.%main 1 #tPi.m)
!! (CHKSUM STAMP)
!! 
MODULE tPi STAMP 0
IMPORT MathL STAMP
IMPORT Out STAMP
ENDHDR

PROC tPi.arctan 28 4 0
! PROCEDURE arctan(arg: LONGREAL): LONGREAL;
!   n := 1; pow := arg; sum := arg;
CONST 1
STLW -28
LDLD 12
STLD -8
LDLD 12
STLD -24
LABEL L1
!     n := n+2;
LDLW -28
CONST 2
PLUS
STLW -28
!     pow := -pow*arg*arg;
LDLD -8
LDLD 12
DTIMES
LDLD 12
DTIMES
DUMINUS
STLD -8
!     term := pow/n;
LDLD -8
LDLW -28
CONVND
DZCHECK 17
DDIV
STLD -16
!     sum := sum+term
LDLD -24
LDLD -16
DPLUS
STLD -24
!   UNTIL ABS(term) < 1.0E-12;
LDLD -16
GLOBAL ABSDOUBLE
CALLD 2
DCONST 1.0e-12
DJNLT L1
!   RETURN sum
LDLD -24
RETURN
END

PROC tPi.%main 0 6 0
!   Out.LongReal(16*arctan(1/5)-4*arctan(1/239)); Out.Ln;
DCONST 0.2
GLOBAL tPi.arctan
CALLD 2
DCONST 16.0
DTIMES
DCONST 0.00418410041841
GLOBAL tPi.arctan
CALLD 2
DCONST 4.0
DTIMES
DMINUS
GLOBAL Out.LongReal
CALL 2
GLOBAL Out.Ln
CALL 0
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
