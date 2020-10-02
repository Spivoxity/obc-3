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
!! (SYMFILE #tSegSum STAMP #tSegSum.%main 1 #tSegSum.m)
!! (CHKSUM STAMP)
!! 
MODULE tSegSum STAMP 0
IMPORT Out STAMP
ENDHDR

PROC tSegSum.f 0 2 0
! PROCEDURE f(i: INTEGER): INTEGER;
!   RETURN i+1
LDLW 12
INC
RETURN
END

PROC tSegSum.%main 0 3 0
!   u := 0; v := 0; count := 0; sum := 0;
CONST 0
STGW tSegSum.u
CONST 0
STGW tSegSum.v
CONST 0
STGW tSegSum.count
CONST 0
STGW tSegSum.sum
LABEL L1
!   WHILE f(u) <= A DO
LDGW tSegSum.u
GLOBAL tSegSum.f
CALLW 1
CONST 120
JGT L3
!     IF sum = A THEN
LDGW tSegSum.sum
CONST 120
JNEQ L6
!       count := count+1;
LDGW tSegSum.count
INC
STGW tSegSum.count
!       sum := sum + f(v) - f(u);
LDGW tSegSum.sum
LDGW tSegSum.v
GLOBAL tSegSum.f
CALLW 1
PLUS
LDGW tSegSum.u
GLOBAL tSegSum.f
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
JUMP L1
LABEL L6
!     ELSIF sum < A THEN
LDGW tSegSum.sum
CONST 120
JGEQ L8
!       sum := sum + f(v);
LDGW tSegSum.sum
LDGW tSegSum.v
GLOBAL tSegSum.f
CALLW 1
PLUS
STGW tSegSum.sum
!       v := v+1
LDGW tSegSum.v
INC
STGW tSegSum.v
JUMP L1
LABEL L8
!       sum := sum - f(u);
LDGW tSegSum.sum
LDGW tSegSum.u
GLOBAL tSegSum.f
CALLW 1
MINUS
STGW tSegSum.sum
!       u := u+1
LDGW tSegSum.u
INC
STGW tSegSum.u
JUMP L1
LABEL L3
!   Out.Int(count, 0); Out.Ln
CONST 0
LDGW tSegSum.count
GLOBAL Out.Int
CALL 2
GLOBAL Out.Ln
CALL 0
RETURN
END

! Global variables
GLOVAR tSegSum.u 4
GLOVAR tSegSum.v 4
GLOVAR tSegSum.count 4
GLOVAR tSegSum.sum 4

! End of file
]]*)
