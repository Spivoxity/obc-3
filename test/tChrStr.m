MODULE tChrStr;

(*<<
xAyzz1
>>*)

IMPORT Out;

TYPE q = ARRAY 2 OF CHAR;

VAR 
  n: INTEGER;
  s: q;
  t: ARRAY 3 OF CHAR;

PROCEDURE p(u: q);
BEGIN
  Out.String(u)
END p;

BEGIN
  s := 'x';
  Out.String(s);
  Out.String(CHR(65));
  t := 'y';
  IF t = 'y' THEN Out.String(t) END;
  p('z');
  n := ORD('z');
  s[0] := CHR(n);
  p(s);
  n := ORD(t[0]);
  n := ORD(CHR(n+257))-n;
  Out.Int(n, 0);
  Out.Ln
END tChrStr.

(*[[
!! SYMFILE #tChrStr STAMP #tChrStr.%main 1
!! END STAMP
!! 
MODULE tChrStr STAMP 0
IMPORT Out STAMP
ENDHDR

PROC tChrStr.p 1 12 0
! PROCEDURE p(u: q);
LOCAL -2
LDLW 12
CONST 2
FIXCOPY
!   Out.String(u)
CONST 2
LOCAL -2
CONST Out.String
CALL 2
RETURN
END

PROC tChrStr.%main 0 20 0
!   s := 'x';
CONST 2
CONST tChrStr.s
CONST 2
CONST tChrStr.%1
CONST COPY
CALL 4
!   Out.String(s);
CONST 2
CONST tChrStr.s
CONST Out.String
CALL 2
!   Out.String(CHR(65));
CONST 2
CONST tChrStr.%2
CONST Out.String
CALL 2
!   t := 'y';
CONST 3
CONST tChrStr.t
CONST 2
CONST tChrStr.%3
CONST COPY
CALL 4
!   IF t = 'y' THEN Out.String(t) END;
CONST 2
CONST tChrStr.%3
CONST 3
CONST tChrStr.t
CONST COMPARE
CALLW 4
JNEQZ 6
CONST 3
CONST tChrStr.t
CONST Out.String
CALL 2
LABEL 6
!   p('z');
CONST tChrStr.%4
CONST tChrStr.p
CALL 1
!   n := ORD('z');
CONST 122
STGW tChrStr.n
!   s[0] := CHR(n);
LDGW tChrStr.n
STGC tChrStr.s
!   p(s);
CONST tChrStr.s
CONST tChrStr.p
CALL 1
!   n := ORD(t[0]);
LDGC tChrStr.t
STGW tChrStr.n
!   n := ORD(CHR(n+257))-n;
LDGW tChrStr.n
CONST 257
PLUS
CONVNC
LDGW tChrStr.n
MINUS
STGW tChrStr.n
!   Out.Int(n, 0);
CONST 0
LDGW tChrStr.n
CONST Out.Int
CALL 2
!   Out.Ln
CONST Out.Ln
CALL 0
RETURN
END

! Global variables
GLOBAL tChrStr.n 4
GLOBAL tChrStr.s 2
GLOBAL tChrStr.t 3

! String "x"
DEFINE tChrStr.%1
STRING 7800

! String "A"
DEFINE tChrStr.%2
STRING 4100

! String "y"
DEFINE tChrStr.%3
STRING 7900

! String "z"
DEFINE tChrStr.%4
STRING 7A00

! End of file
]]*)
