MODULE tLongInt07;

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

  n := 7FFFFFFFH; 
  n := n+n;
  n := n+n;
  Out.LongInt(n, 0); Out.Ln;
  n := n - 7FFFFFFFH;
  Out.LongInt(-n, 0); Out.Ln;

  m := 7FFFFFFFH;
  INC(m);
  Out.LongInt(m, 0); Out.Ln;
  IF m >= 0 THEN Out.String("Good!") ELSE Out.String("Bad!") END; Out.Ln
END tLongInt07.

(*<<
2000000000000
1099511627776
8589934588
-6442450941
2147483648
Good!
>>*)

(*[[
!! (SYMFILE #tLongInt07 STAMP #tLongInt07.%main 1 #tLongInt07.m)
!! (CHKSUM STAMP)
!! 
MODULE tLongInt07 STAMP 0
IMPORT Out STAMP
ENDHDR

PROC tLongInt07.Square 0 4 0
! PROCEDURE Square(x: LONGINT): LONGINT;
!   RETURN x * x
LDLQ 12
LDLQ 12
QTIMES
RETURN
END

PROC tLongInt07.%main 0 5 0
!   r := 2;
CONST 2
STGW tLongInt07.r
!   n := 1000;
CONST 1000
CONVNQ
STGQ tLongInt07.n
!   n := Square(n * n);
LDGQ tLongInt07.n
LDGQ tLongInt07.n
QTIMES
GLOBAL tLongInt07.Square
CALLQ 2
STGQ tLongInt07.n
!   Out.LongInt(n + n, 0); Out.Ln;
CONST 0
LDGQ tLongInt07.n
LDGQ tLongInt07.n
QPLUS
GLOBAL Out.LongInt
CALL 3
GLOBAL Out.Ln
CALL 0
!   m := 1; k := 0;
CONST 1
CONVNQ
STGQ tLongInt07.m
CONST 0
STGW tLongInt07.k
LABEL L3
!   WHILE m < n DO 
LDGQ tLongInt07.m
LDGQ tLongInt07.n
QJGEQ L5
!     m := r * m;
LDGW tLongInt07.r
CONVNQ
LDGQ tLongInt07.m
QTIMES
STGQ tLongInt07.m
!     k := k + 1 
LDGW tLongInt07.k
INC
STGW tLongInt07.k
JUMP L3
LABEL L5
!   Out.LongInt(m, 0); Out.Ln;
CONST 0
LDGQ tLongInt07.m
GLOBAL Out.LongInt
CALL 3
GLOBAL Out.Ln
CALL 0
!   n := 7FFFFFFFH; 
QCONST 2147483647
STGQ tLongInt07.n
!   n := n+n;
LDGQ tLongInt07.n
LDGQ tLongInt07.n
QPLUS
STGQ tLongInt07.n
!   n := n+n;
LDGQ tLongInt07.n
LDGQ tLongInt07.n
QPLUS
STGQ tLongInt07.n
!   Out.LongInt(n, 0); Out.Ln;
CONST 0
LDGQ tLongInt07.n
GLOBAL Out.LongInt
CALL 3
GLOBAL Out.Ln
CALL 0
!   n := n - 7FFFFFFFH;
LDGQ tLongInt07.n
QCONST 2147483647
QMINUS
STGQ tLongInt07.n
!   Out.LongInt(-n, 0); Out.Ln;
CONST 0
LDGQ tLongInt07.n
QUMINUS
GLOBAL Out.LongInt
CALL 3
GLOBAL Out.Ln
CALL 0
!   m := 7FFFFFFFH;
QCONST 2147483647
STGQ tLongInt07.m
!   INC(m);
CONST 1
CONVNQ
GLOBAL tLongInt07.m
GLOBAL INCLONG
CALL 3
!   Out.LongInt(m, 0); Out.Ln;
CONST 0
LDGQ tLongInt07.m
GLOBAL Out.LongInt
CALL 3
GLOBAL Out.Ln
CALL 0
!   IF m >= 0 THEN Out.String("Good!") ELSE Out.String("Bad!") END; Out.Ln
LDGQ tLongInt07.m
CONST 0
CONVNQ
QJLT L8
CONST 6
GLOBAL tLongInt07.%1
GLOBAL Out.String
CALL 2
JUMP L6
LABEL L8
CONST 5
GLOBAL tLongInt07.%2
GLOBAL Out.String
CALL 2
LABEL L6
GLOBAL Out.Ln
CALL 0
RETURN
END

! Global variables
GLOVAR tLongInt07.r 4
GLOVAR tLongInt07.k 4
GLOVAR tLongInt07.m 8
GLOVAR tLongInt07.n 8

! String "Good!"
DEFINE tLongInt07.%1
STRING 476F6F642100

! String "Bad!"
DEFINE tLongInt07.%2
STRING 4261642100

! End of file
]]*)
