MODULE tRecurse;

(* Test for stack growth -- requires about 4MB of stack space *)

(*<<
20100
800020000
>>*)

IMPORT Out;

PROCEDURE Sum(n: INTEGER; a: ARRAY OF INTEGER): INTEGER;
BEGIN
  (* The flex parameter is copied into space allocated below
     the activation record *)
  IF n = 0 THEN
    RETURN 0
  ELSE
    RETURN a[n-1] + Sum(n-1, a)
  END
END Sum;

PROCEDURE Sumorial(n: INTEGER): INTEGER;
BEGIN
  IF n = 0 THEN
    RETURN 0
  ELSE
    RETURN n + Sumorial(n-1)
  END
END Sumorial;

CONST N = 200;

VAR n: INTEGER; a: ARRAY N OF INTEGER;

BEGIN
  FOR n := 0 TO N-1 DO a[n] := N-n END;
  Out.Int(Sum(N, a), 0); Out.Ln;
  Out.Int(Sumorial(40000), 0); Out.Ln
END tRecurse.

(*[[
!! SYMFILE #tRecurse STAMP #tRecurse.%main 1
!! END STAMP
!! 
MODULE tRecurse STAMP 0
IMPORT Out STAMP
ENDHDR

PROC tRecurse.Sum 0 20 0
! PROCEDURE Sum(n: INTEGER; a: ARRAY OF INTEGER): INTEGER;
LOCAL 16
LDLW 20
CONST 4
TIMES
FLEXCOPY
!   IF n = 0 THEN
LDLW 12
JNEQZ 2
!     RETURN 0
CONST 0
RETURNW
LABEL 2
!     RETURN a[n-1] + Sum(n-1, a)
LDLW 16
LDLW 12
DEC
LDLW 20
BOUND 19
LDIW
LDLW 20
LDLW 16
LDLW 12
DEC
CONST tRecurse.Sum
CALLW 3
PLUS
RETURNW
END

PROC tRecurse.Sumorial 0 20 0
! PROCEDURE Sumorial(n: INTEGER): INTEGER;
!   IF n = 0 THEN
LDLW 12
JNEQZ 4
!     RETURN 0
CONST 0
RETURNW
LABEL 4
!     RETURN n + Sumorial(n-1)
LDLW 12
LDLW 12
DEC
CONST tRecurse.Sumorial
CALLW 1
PLUS
RETURNW
END

PROC tRecurse.%main 0 20 0
!   FOR n := 0 TO N-1 DO a[n] := N-n END;
CONST 0
STGW tRecurse.n
JUMP 6
LABEL 5
CONST 200
LDGW tRecurse.n
MINUS
CONST tRecurse.a
LDGW tRecurse.n
CONST 200
BOUND 37
STIW
LDGW tRecurse.n
INC
STGW tRecurse.n
LABEL 6
LDGW tRecurse.n
CONST 199
JLEQ 5
!   Out.Int(Sum(N, a), 0); Out.Ln;
CONST 0
CONST 200
CONST tRecurse.a
CONST 200
CONST tRecurse.Sum
CALLW 3
CONST Out.Int
CALL 2
CONST Out.Ln
CALL 0
!   Out.Int(Sumorial(40000), 0); Out.Ln
CONST 0
CONST 40000
CONST tRecurse.Sumorial
CALLW 1
CONST Out.Int
CALL 2
CONST Out.Ln
CALL 0
RETURN
END

! Global variables
GLOBAL tRecurse.n 4
GLOBAL tRecurse.a 800

! End of file
]]*)
