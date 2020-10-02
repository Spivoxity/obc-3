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
!! (SYMFILE #tChrStr STAMP #tChrStr.%main 1 #tChrStr.m)
!! (CHKSUM STAMP)
!! 
MODULE tChrStr STAMP 0
IMPORT Out STAMP
ENDHDR

PROC tChrStr.p 4 3 0
! PROCEDURE p(u: q);
LOCAL -2
LDLW 12
CONST 2
FIXCOPY
!   Out.String(u)
CONST 2
LOCAL -2
GLOBAL Out.String
CALL 2
RETURN
END

PROC tChrStr.%main 0 5 0
!   s := 'x';
CONST 2
GLOBAL tChrStr.s
CONST 2
GLOBAL tChrStr.%1
GLOBAL COPY
CALL 4
!   Out.String(s);
CONST 2
GLOBAL tChrStr.s
GLOBAL Out.String
CALL 2
!   Out.String(CHR(65));
CONST 2
GLOBAL tChrStr.%2
GLOBAL Out.String
CALL 2
!   t := 'y';
CONST 3
GLOBAL tChrStr.t
CONST 2
GLOBAL tChrStr.%3
GLOBAL COPY
CALL 4
!   IF t = 'y' THEN Out.String(t) END;
CONST 2
GLOBAL tChrStr.%3
CONST 3
GLOBAL tChrStr.t
GLOBAL COMPARE
CALLW 4
JNEQZ L7
CONST 3
GLOBAL tChrStr.t
GLOBAL Out.String
CALL 2
LABEL L7
!   p('z');
GLOBAL tChrStr.%4
GLOBAL tChrStr.p
CALL 1
!   n := ORD('z');
CONST 122
STGW tChrStr.n
!   s[0] := CHR(n);
LDGW tChrStr.n
STGC tChrStr.s
!   p(s);
GLOBAL tChrStr.s
GLOBAL tChrStr.p
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
GLOBAL Out.Int
CALL 2
!   Out.Ln
GLOBAL Out.Ln
CALL 0
RETURN
END

! Global variables
GLOVAR tChrStr.n 4
GLOVAR tChrStr.s 2
GLOVAR tChrStr.t 3

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
