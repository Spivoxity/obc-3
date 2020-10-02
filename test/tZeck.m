(*
let S(n) = sum_{0 < k < n} z(n)

S(1) = 0
S(2) = 1
S(3) = 2

S(F_n) = S(F_{n-1}) + sum_{0 <= i < F_{n-2}} (1 + z(i))
  = S(F_{n-1}) + S(F_{n-2}) + F_{n-2}

S(F_n + r) = S(F_n) + S(r) + r for r < F_{n-1}
*)

MODULE tZeck;

IMPORT Out;

CONST ten17 = 100000000000000000;

VAR fib, sum: ARRAY 100 OF LONGINT;

PROCEDURE Sum(n: LONGINT): LONGINT;
  VAR j: INTEGER; r: LONGINT;
BEGIN
  IF n = 0 THEN RETURN 0 END;

  j := 0;
  WHILE fib[j+1] <= n DO j := j+1 END;
  r := n - fib[j];
  RETURN sum[j] + Sum(r) + r
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

END tZeck.

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
!! (SYMFILE #tZeck STAMP #tZeck.%main 14 #tZeck.m)
!! (CHKSUM STAMP)
!! 
MODULE tZeck STAMP 0
IMPORT Out STAMP
ENDHDR

PROC tZeck.Sum 12 5 0
! PROCEDURE Sum(n: LONGINT): LONGINT;
!   IF n = 0 THEN RETURN 0 END;
LDLQ 12
CONST 0
CONVNQ
QJNEQ L3
CONST 0
CONVNQ
RETURN
LABEL L3
!   j := 0;
CONST 0
STLW -4
LABEL L4
!   WHILE fib[j+1] <= n DO j := j+1 END;
GLOBAL tZeck.fib
LDLW -4
INC
CONST 100
BOUND 28
LDIQ
LDLQ 12
QJGT L6
INCL -4
JUMP L4
LABEL L6
!   r := n - fib[j];
LDLQ 12
GLOBAL tZeck.fib
LDLW -4
CONST 100
BOUND 29
LDIQ
QMINUS
STLQ -12
!   RETURN sum[j] + Sum(r) + r
GLOBAL tZeck.sum
LDLW -4
CONST 100
BOUND 30
LDIQ
LDLQ -12
GLOBAL tZeck.Sum
CALLQ 2
QPLUS
LDLQ -12
QPLUS
RETURN
END

PROC tZeck.%main 0 5 0
!   fib[0] := 1; fib[1] := 2;
CONST 1
CONVNQ
STGQ tZeck.fib
CONST 2
CONVNQ
GLOBAL tZeck.fib
CONST 1
STIQ
!   sum[0] := 0; sum[1] := 1;
CONST 0
CONVNQ
STGQ tZeck.sum
CONST 1
CONVNQ
GLOBAL tZeck.sum
CONST 1
STIQ
!   M := 1;
CONST 1
STGW tZeck.M
LABEL L7
!     M := M+1; 
LDGW tZeck.M
INC
STGW tZeck.M
!     fib[M] := fib[M-1] + fib[M-2];
GLOBAL tZeck.fib
LDGW tZeck.M
DEC
CONST 100
BOUND 41
LDIQ
GLOBAL tZeck.fib
LDGW tZeck.M
CONST 2
MINUS
CONST 100
BOUND 41
LDIQ
QPLUS
GLOBAL tZeck.fib
LDGW tZeck.M
CONST 100
BOUND 41
STIQ
!     sum[M] := sum[M-1] + sum[M-2] + fib[M-2]
GLOBAL tZeck.sum
LDGW tZeck.M
DEC
CONST 100
BOUND 42
LDIQ
GLOBAL tZeck.sum
LDGW tZeck.M
CONST 2
MINUS
CONST 100
BOUND 42
LDIQ
QPLUS
GLOBAL tZeck.fib
LDGW tZeck.M
CONST 2
MINUS
CONST 100
BOUND 42
LDIQ
QPLUS
GLOBAL tZeck.sum
LDGW tZeck.M
CONST 100
BOUND 42
STIQ
!   UNTIL fib[M] > ten17;
GLOBAL tZeck.fib
LDGW tZeck.M
CONST 100
BOUND 43
LDIQ
QCONST 100000000000000000
QJLEQ L7
!   Out.LongInt(Sum(1), 0); Out.Ln;
CONST 0
CONST 1
CONVNQ
GLOBAL tZeck.Sum
CALLQ 2
GLOBAL Out.LongInt
CALL 3
GLOBAL Out.Ln
CALL 0
!   Out.LongInt(Sum(2), 0); Out.Ln;
CONST 0
CONST 2
CONVNQ
GLOBAL tZeck.Sum
CALLQ 2
GLOBAL Out.LongInt
CALL 3
GLOBAL Out.Ln
CALL 0
!   Out.LongInt(Sum(3), 0); Out.Ln;
CONST 0
CONST 3
CONVNQ
GLOBAL tZeck.Sum
CALLQ 2
GLOBAL Out.LongInt
CALL 3
GLOBAL Out.Ln
CALL 0
!   Out.LongInt(Sum(4), 0); Out.Ln;
CONST 0
CONST 4
CONVNQ
GLOBAL tZeck.Sum
CALLQ 2
GLOBAL Out.LongInt
CALL 3
GLOBAL Out.Ln
CALL 0
!   Out.LongInt(Sum(5), 0); Out.Ln;
CONST 0
CONST 5
CONVNQ
GLOBAL tZeck.Sum
CALLQ 2
GLOBAL Out.LongInt
CALL 3
GLOBAL Out.Ln
CALL 0
!   Out.LongInt(Sum(1000000), 0); Out.Ln;
CONST 0
QCONST 1000000
GLOBAL tZeck.Sum
CALLQ 2
GLOBAL Out.LongInt
CALL 3
GLOBAL Out.Ln
CALL 0
!   Out.LongInt(Sum(ten17), 0); Out.Ln
CONST 0
QCONST 100000000000000000
GLOBAL tZeck.Sum
CALLQ 2
GLOBAL Out.LongInt
CALL 3
GLOBAL Out.Ln
CALL 0
RETURN
END

! Global variables
GLOVAR tZeck.fib 800
GLOVAR tZeck.sum 800
GLOVAR tZeck.M 4

! End of file
]]*)
