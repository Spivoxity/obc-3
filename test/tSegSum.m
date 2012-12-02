MODULE tSegSum;

IMPORT Out;

PROCEDURE f(i: INTEGER): INTEGER;
BEGIN
  RETURN i+1
END f;

CONST A = 120;

VAR u, v, count, sum: INTEGER;

BEGIN
  u := 0; v := 0; count := 0; sum := 0;
  WHILE f(u) <= A DO
    IF sum = A THEN
      count := count+1;
      sum := sum + f(v) - f(u);
      u := u+1; v := v+1
    ELSIF sum < A THEN
      sum := sum + f(v);
      v := v+1
    ELSE
      sum := sum - f(u);
      u := u+1
    END
  END;

  Out.Int(count, 0); Out.Ln
END tSegSum.

(*<<
4
>>*)

(*[[
!! SYMFILE #tSegSum STAMP #tSegSum.%main 1
!! END STAMP
!! 
MODULE tSegSum STAMP 0
IMPORT Out STAMP
ENDHDR

PROC tSegSum.f 0 8 0
! PROCEDURE f(i: INTEGER): INTEGER;
!   RETURN i+1
LDLW 12
INC
RETURNW
END

PROC tSegSum.%main 0 12 0
!   u := 0; v := 0; count := 0; sum := 0;
CONST 0
STGW tSegSum.u
CONST 0
STGW tSegSum.v
CONST 0
STGW tSegSum.count
CONST 0
STGW tSegSum.sum
JUMP 3
LABEL 1
!     IF sum = A THEN
LDGW tSegSum.sum
CONST 120
JNEQ 4
!       count := count+1;
LDGW tSegSum.count
INC
STGW tSegSum.count
!       sum := sum + f(v) - f(u);
LDGW tSegSum.sum
LDGW tSegSum.v
CONST tSegSum.f
CALLW 1
PLUS
LDGW tSegSum.u
CONST tSegSum.f
CALLW 1
MINUS
STGW tSegSum.sum
!       u := u+1; v := v+1
LDGW tSegSum.u
INC
STGW tSegSum.u
LDGW tSegSum.v
INC
STGW tSegSum.v
JUMP 3
LABEL 4
!     ELSIF sum < A THEN
LDGW tSegSum.sum
CONST 120
JGEQ 5
!       sum := sum + f(v);
LDGW tSegSum.sum
LDGW tSegSum.v
CONST tSegSum.f
CALLW 1
PLUS
STGW tSegSum.sum
!       v := v+1
LDGW tSegSum.v
INC
STGW tSegSum.v
JUMP 3
LABEL 5
!       sum := sum - f(u);
LDGW tSegSum.sum
LDGW tSegSum.u
CONST tSegSum.f
CALLW 1
MINUS
STGW tSegSum.sum
!       u := u+1
LDGW tSegSum.u
INC
STGW tSegSum.u
LABEL 3
!   WHILE f(u) <= A DO
LDGW tSegSum.u
CONST tSegSum.f
CALLW 1
CONST 120
JLEQ 1
!   Out.Int(count, 0); Out.Ln
CONST 0
LDGW tSegSum.count
CONST Out.Int
CALL 2
CONST Out.Ln
CALL 0
RETURN
END

! Global variables
GLOBAL tSegSum.u 4
GLOBAL tSegSum.v 4
GLOBAL tSegSum.count 4
GLOBAL tSegSum.sum 4

! End of file
]]*)
