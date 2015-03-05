MODULE tSegSum07;

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
END tSegSum07.

(*<<
4
>>*)

(*[[
!! SYMFILE #tSegSum07 STAMP #tSegSum07.%main 1
!! END STAMP
!! 
MODULE tSegSum07 STAMP 0
IMPORT Out STAMP
ENDHDR

PROC tSegSum07.f 0 2 0
! PROCEDURE f(i: INTEGER): INTEGER;
!   RETURN i+1
LDLW 12
INC
RETURNW
END

PROC tSegSum07.%main 0 3 0
!   u := 0; v := 0; count := 0; sum := 0;
CONST 0
STGW tSegSum07.u
CONST 0
STGW tSegSum07.v
CONST 0
STGW tSegSum07.count
CONST 0
STGW tSegSum07.sum
LABEL 1
!   WHILE f(u) <= A DO
LDGW tSegSum07.u
GLOBAL tSegSum07.f
CALLW 1
CONST 120
JGT 3
!     IF sum = A THEN
LDGW tSegSum07.sum
CONST 120
JNEQ 6
!       count := count+1;
LDGW tSegSum07.count
INC
STGW tSegSum07.count
!       sum := sum + f(v) - f(u);
LDGW tSegSum07.sum
LDGW tSegSum07.v
GLOBAL tSegSum07.f
CALLW 1
PLUS
LDGW tSegSum07.u
GLOBAL tSegSum07.f
CALLW 1
MINUS
STGW tSegSum07.sum
!       u := u+1; v := v+1
LDGW tSegSum07.u
INC
STGW tSegSum07.u
LDGW tSegSum07.v
INC
STGW tSegSum07.v
JUMP 1
LABEL 6
!     ELSIF sum < A THEN
LDGW tSegSum07.sum
CONST 120
JGEQ 8
!       sum := sum + f(v);
LDGW tSegSum07.sum
LDGW tSegSum07.v
GLOBAL tSegSum07.f
CALLW 1
PLUS
STGW tSegSum07.sum
!       v := v+1
LDGW tSegSum07.v
INC
STGW tSegSum07.v
JUMP 1
LABEL 8
!       sum := sum - f(u);
LDGW tSegSum07.sum
LDGW tSegSum07.u
GLOBAL tSegSum07.f
CALLW 1
MINUS
STGW tSegSum07.sum
!       u := u+1
LDGW tSegSum07.u
INC
STGW tSegSum07.u
JUMP 1
LABEL 3
!   Out.Int(count, 0); Out.Ln
CONST 0
LDGW tSegSum07.count
GLOBAL Out.Int
CALL 2
GLOBAL Out.Ln
CALL 0
RETURN
END

! Global variables
GLOVAR tSegSum07.u 4
GLOVAR tSegSum07.v 4
GLOVAR tSegSum07.count 4
GLOVAR tSegSum07.sum 4

! End of file
]]*)
