MODULE tLongInt;

IMPORT Out;

VAR r, k: INTEGER; m, n: LONGINT;

PROCEDURE Square(x: LONGINT): LONGINT;
BEGIN
  RETURN x * x
END Square;

BEGIN
  r := 2;
  n := 1000;
  n := Square(n * n);
  Out.LongInt(n + n, 0); Out.Ln;

  m := 1; k := 0;
  WHILE m < n DO 
    m := r * m;
    k := k + 1 
  END;
  Out.LongInt(m, 0); Out.Ln;

  n := MAX(INTEGER); 
  n := n+n;
  n := n+n;
  Out.LongInt(n, 0); Out.Ln;
  n := n - MAX(INTEGER);
  Out.LongInt(-n, 0); Out.Ln;

  m := MAX(INTEGER);
  INC(m);
  Out.LongInt(m, 0); Out.Ln;
  IF m >= 0 THEN Out.String("Good!") ELSE Out.String("Bad!") END; Out.Ln
END tLongInt.

(*<<
2000000000000
1099511627776
8589934588
-6442450941
2147483648
Good!
>>*)

(*[[
!! SYMFILE #tLongInt STAMP #tLongInt.%main 1
!! END STAMP
!! 
MODULE tLongInt STAMP 0
IMPORT Out STAMP
ENDHDR

PROC tLongInt.Square 0 16 0
! PROCEDURE Square(x: LONGINT): LONGINT;
!   RETURN x * x
LDLQ 12
LDLQ 12
QTIMES
RETURNQ
END

PROC tLongInt.%main 0 24 0
!   r := 2;
CONST 2
STGW tLongInt.r
!   n := 1000;
CONST 1000
CONVNQ
STGQ tLongInt.n
!   n := Square(n * n);
LDGQ tLongInt.n
LDGQ tLongInt.n
QTIMES
CONST tLongInt.Square
CALLQ 2
STGQ tLongInt.n
!   Out.LongInt(n + n, 0); Out.Ln;
CONST 0
LDGQ tLongInt.n
LDGQ tLongInt.n
QPLUS
CONST Out.LongInt
CALL 3
CONST Out.Ln
CALL 0
!   m := 1; k := 0;
CONST 1
CONVNQ
STGQ tLongInt.m
CONST 0
STGW tLongInt.k
JUMP 4
LABEL 3
!     m := r * m;
LDGW tLongInt.r
CONVNQ
LDGQ tLongInt.m
QTIMES
STGQ tLongInt.m
!     k := k + 1 
LDGW tLongInt.k
INC
STGW tLongInt.k
LABEL 4
!   WHILE m < n DO 
LDGQ tLongInt.m
LDGQ tLongInt.n
QJLT 3
!   Out.LongInt(m, 0); Out.Ln;
CONST 0
LDGQ tLongInt.m
CONST Out.LongInt
CALL 3
CONST Out.Ln
CALL 0
!   n := MAX(INTEGER); 
QCONST 2147483647
STGQ tLongInt.n
!   n := n+n;
LDGQ tLongInt.n
LDGQ tLongInt.n
QPLUS
STGQ tLongInt.n
!   n := n+n;
LDGQ tLongInt.n
LDGQ tLongInt.n
QPLUS
STGQ tLongInt.n
!   Out.LongInt(n, 0); Out.Ln;
CONST 0
LDGQ tLongInt.n
CONST Out.LongInt
CALL 3
CONST Out.Ln
CALL 0
!   n := n - MAX(INTEGER);
LDGQ tLongInt.n
QCONST 2147483647
QMINUS
STGQ tLongInt.n
!   Out.LongInt(-n, 0); Out.Ln;
CONST 0
LDGQ tLongInt.n
QUMINUS
CONST Out.LongInt
CALL 3
CONST Out.Ln
CALL 0
!   m := MAX(INTEGER);
QCONST 2147483647
STGQ tLongInt.m
!   INC(m);
CONST 1
CONVNQ
CONST tLongInt.m
CONST INCLONG
CALLQ 3
!   Out.LongInt(m, 0); Out.Ln;
CONST 0
LDGQ tLongInt.m
CONST Out.LongInt
CALL 3
CONST Out.Ln
CALL 0
!   IF m >= 0 THEN Out.String("Good!") ELSE Out.String("Bad!") END; Out.Ln
LDGQ tLongInt.m
CONST 0
CONVNQ
QJLT 6
CONST 6
CONST tLongInt.%1
CONST Out.String
CALL 2
JUMP 5
LABEL 6
CONST 5
CONST tLongInt.%2
CONST Out.String
CALL 2
LABEL 5
CONST Out.Ln
CALL 0
RETURN
END

! Global variables
GLOBAL tLongInt.r 4
GLOBAL tLongInt.k 4
GLOBAL tLongInt.m 8
GLOBAL tLongInt.n 8

! String "Good!"
DEFINE tLongInt.%1
STRING 476F6F642100

! String "Bad!"
DEFINE tLongInt.%2
STRING 4261642100

! End of file
]]*)
