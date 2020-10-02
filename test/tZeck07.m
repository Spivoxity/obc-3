(*
let S(n) = sum_{0 < k < n} z(n)

S(1) = 0
S(2) = 1
S(3) = 2

S(F_n) = S(F_{n-1}) + sum_{0 <= i < F_{n-2}} (1 + z(i))
  = S(F_{n-1}) + S(F_{n-2}) + F_{n-2}

S(F_n + r) = S(F_n) + S(r) + r for r < F_{n-1}
*)

MODULE tZeck07;

IMPORT Out;

CONST ten17 = 100000000000000000;

VAR fib, sum: ARRAY 100 OF LONGINT;

PROCEDURE Sum(n: LONGINT): LONGINT;
  VAR j: INTEGER; r: LONGINT;
BEGIN
  IF n = 0 THEN 
    r := 0
  ELSE
    j := 0;
    WHILE fib[j+1] <= n DO j := j+1 END;
    r := n - fib[j];
    r := sum[j] + Sum(r) + r
  END
RETURN r
END Sum;

VAR M: INTEGER;

BEGIN
  fib[0] := 1; fib[1] := 2;
  sum[0] := 0; sum[1] := 1;
  M := 1;
  REPEAT 
    M := M+1; 
    fib[M] := fib[M-1] + fib[M-2];
    sum[M] := sum[M-1] + sum[M-2] + fib[M-2]
  UNTIL fib[M] > ten17;

  Out.LongInt(Sum(1), 0); Out.Ln;
  Out.LongInt(Sum(2), 0); Out.Ln;
  Out.LongInt(Sum(3), 0); Out.Ln;
  Out.LongInt(Sum(4), 0); Out.Ln;
  Out.LongInt(Sum(5), 0); Out.Ln;

  Out.LongInt(Sum(1000000), 0); Out.Ln;
  Out.LongInt(Sum(ten17), 0); Out.Ln

(*
  FOR i := 0 TO M DO
    Out.Int(i, 4); Out.Char(' '); Out.LongInt(fib[i], 0); Out.Ln
  END
*)

END tZeck07.

(*<<
0
1
2
3
5
7894453
2252639041804718029
>>*)

(*[[
!! (SYMFILE #tZeck07 STAMP #tZeck07.%main 14 #tZeck07.m)
!! (CHKSUM STAMP)
!! 
MODULE tZeck07 STAMP 0
IMPORT Out STAMP
ENDHDR

PROC tZeck07.Sum 12 5 0
! PROCEDURE Sum(n: LONGINT): LONGINT;
!   IF n = 0 THEN 
LDLQ 12
CONST 0
CONVNQ
QJNEQ L6
!     r := 0
CONST 0
CONVNQ
STLQ -12
JUMP L1
LABEL L6
!     j := 0;
CONST 0
STLW -4
LABEL L2
!     WHILE fib[j+1] <= n DO j := j+1 END;
GLOBAL tZeck07.fib
LDLW -4
INC
CONST 100
BOUND 29
LDIQ
LDLQ 12
QJGT L4
INCL -4
JUMP L2
LABEL L4
!     r := n - fib[j];
LDLQ 12
GLOBAL tZeck07.fib
LDLW -4
CONST 100
BOUND 30
LDIQ
QMINUS
STLQ -12
!     r := sum[j] + Sum(r) + r
GLOBAL tZeck07.sum
LDLW -4
CONST 100
BOUND 31
LDIQ
LDLQ -12
GLOBAL tZeck07.Sum
CALLQ 2
QPLUS
LDLQ -12
QPLUS
STLQ -12
LABEL L1
! RETURN r
LDLQ -12
RETURN
END

PROC tZeck07.%main 0 5 0
!   fib[0] := 1; fib[1] := 2;
CONST 1
CONVNQ
STGQ tZeck07.fib
CONST 2
CONVNQ
GLOBAL tZeck07.fib
CONST 1
STIQ
!   sum[0] := 0; sum[1] := 1;
CONST 0
CONVNQ
STGQ tZeck07.sum
CONST 1
CONVNQ
GLOBAL tZeck07.sum
CONST 1
STIQ
!   M := 1;
CONST 1
STGW tZeck07.M
LABEL L7
!     M := M+1; 
LDGW tZeck07.M
INC
STGW tZeck07.M
!     fib[M] := fib[M-1] + fib[M-2];
GLOBAL tZeck07.fib
LDGW tZeck07.M
DEC
CONST 100
BOUND 44
LDIQ
GLOBAL tZeck07.fib
LDGW tZeck07.M
CONST 2
MINUS
CONST 100
BOUND 44
LDIQ
QPLUS
GLOBAL tZeck07.fib
LDGW tZeck07.M
CONST 100
BOUND 44
STIQ
!     sum[M] := sum[M-1] + sum[M-2] + fib[M-2]
GLOBAL tZeck07.sum
LDGW tZeck07.M
DEC
CONST 100
BOUND 45
LDIQ
GLOBAL tZeck07.sum
LDGW tZeck07.M
CONST 2
MINUS
CONST 100
BOUND 45
LDIQ
QPLUS
GLOBAL tZeck07.fib
LDGW tZeck07.M
CONST 2
MINUS
CONST 100
BOUND 45
LDIQ
QPLUS
GLOBAL tZeck07.sum
LDGW tZeck07.M
CONST 100
BOUND 45
STIQ
!   UNTIL fib[M] > ten17;
GLOBAL tZeck07.fib
LDGW tZeck07.M
CONST 100
BOUND 46
LDIQ
QCONST 100000000000000000
QJLEQ L7
!   Out.LongInt(Sum(1), 0); Out.Ln;
CONST 0
CONST 1
CONVNQ
GLOBAL tZeck07.Sum
CALLQ 2
GLOBAL Out.LongInt
CALL 3
GLOBAL Out.Ln
CALL 0
!   Out.LongInt(Sum(2), 0); Out.Ln;
CONST 0
CONST 2
CONVNQ
GLOBAL tZeck07.Sum
CALLQ 2
GLOBAL Out.LongInt
CALL 3
GLOBAL Out.Ln
CALL 0
!   Out.LongInt(Sum(3), 0); Out.Ln;
CONST 0
CONST 3
CONVNQ
GLOBAL tZeck07.Sum
CALLQ 2
GLOBAL Out.LongInt
CALL 3
GLOBAL Out.Ln
CALL 0
!   Out.LongInt(Sum(4), 0); Out.Ln;
CONST 0
CONST 4
CONVNQ
GLOBAL tZeck07.Sum
CALLQ 2
GLOBAL Out.LongInt
CALL 3
GLOBAL Out.Ln
CALL 0
!   Out.LongInt(Sum(5), 0); Out.Ln;
CONST 0
CONST 5
CONVNQ
GLOBAL tZeck07.Sum
CALLQ 2
GLOBAL Out.LongInt
CALL 3
GLOBAL Out.Ln
CALL 0
!   Out.LongInt(Sum(1000000), 0); Out.Ln;
CONST 0
QCONST 1000000
GLOBAL tZeck07.Sum
CALLQ 2
GLOBAL Out.LongInt
CALL 3
GLOBAL Out.Ln
CALL 0
!   Out.LongInt(Sum(ten17), 0); Out.Ln
CONST 0
QCONST 100000000000000000
GLOBAL tZeck07.Sum
CALLQ 2
GLOBAL Out.LongInt
CALL 3
GLOBAL Out.Ln
CALL 0
RETURN
END

! Global variables
GLOVAR tZeck07.fib 800
GLOVAR tZeck07.sum 800
GLOVAR tZeck07.M 4

! End of file
]]*)
