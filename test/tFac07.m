MODULE tFac07;

(*<<
The factorial of 10 is 3628800
>>*)

(**)

IMPORT Out;

PROCEDURE fac(n: INTEGER): INTEGER;
  VAR f: INTEGER;
BEGIN 
  IF n = 0 THEN 
    f := 1
  ELSE 
    f := n * fac(n-1)
  END
RETURN f
END fac;

CONST nnn = 10;

BEGIN
  Out.String("The factorial of "); Out.Int(nnn, 0);
  Out.String(" is "); Out.Int(fac(nnn), 0); Out.Ln
END tFac07.

(*[[
!! (SYMFILE #tFac07 STAMP #tFac07.%main 1 #tFac07.m)
!! (CHKSUM STAMP)
!! 
MODULE tFac07 STAMP 0
IMPORT Out STAMP
ENDHDR

PROC tFac07.fac 4 3 0
! PROCEDURE fac(n: INTEGER): INTEGER;
!   IF n = 0 THEN 
LDLW 12
JNEQZ L5
!     f := 1
CONST 1
STLW -4
JUMP L3
LABEL L5
!     f := n * fac(n-1)
LDLW 12
LDLW 12
DEC
GLOBAL tFac07.fac
CALLW 1
TIMES
STLW -4
LABEL L3
! RETURN f
LDLW -4
RETURN
END

PROC tFac07.%main 0 3 0
!   Out.String("The factorial of "); Out.Int(nnn, 0);
CONST 18
GLOBAL tFac07.%1
GLOBAL Out.String
CALL 2
CONST 0
CONST 10
GLOBAL Out.Int
CALL 2
!   Out.String(" is "); Out.Int(fac(nnn), 0); Out.Ln
CONST 5
GLOBAL tFac07.%2
GLOBAL Out.String
CALL 2
CONST 0
CONST 10
GLOBAL tFac07.fac
CALLW 1
GLOBAL Out.Int
CALL 2
GLOBAL Out.Ln
CALL 0
RETURN
END

! String "The factorial of "
DEFINE tFac07.%1
STRING 54686520666163746F7269616C206F662000

! String " is "
DEFINE tFac07.%2
STRING 2069732000

! End of file
]]*)
