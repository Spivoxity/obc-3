MODULE tChrStr07;

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
END tChrStr07.

(*[[
!! (SYMFILE #tChrStr07 STAMP #tChrStr07.%main 1 #tChrStr07.m)
!! (CHKSUM STAMP)
!! 
MODULE tChrStr07 STAMP 0
IMPORT Out STAMP
ENDHDR

PROC tChrStr07.p 0 3 0x00100001
! PROCEDURE p(u: q);
!   Out.String(u)
CONST 2
LDLW 12
GLOBAL Out.String
CALL 2
RETURN
END

PROC tChrStr07.%main 0 5 0
!   s := 'x';
CONST 2
GLOBAL tChrStr07.s
CONST 2
GLOBAL tChrStr07.%1
GLOBAL COPY
CALL 4
!   Out.String(s);
CONST 2
GLOBAL tChrStr07.s
GLOBAL Out.String
CALL 2
!   Out.String(CHR(65));
CONST 2
GLOBAL tChrStr07.%2
GLOBAL Out.String
CALL 2
!   t := 'y';
CONST 3
GLOBAL tChrStr07.t
CONST 2
GLOBAL tChrStr07.%3
GLOBAL COPY
CALL 4
!   IF t = 'y' THEN Out.String(t) END;
CONST 2
GLOBAL tChrStr07.%3
CONST 3
GLOBAL tChrStr07.t
GLOBAL COMPARE
CALLW 4
JNEQZ L7
CONST 3
GLOBAL tChrStr07.t
GLOBAL Out.String
CALL 2
LABEL L7
!   p('z');
GLOBAL tChrStr07.%4
GLOBAL tChrStr07.p
CALL 1
!   n := ORD('z');
CONST 122
STGW tChrStr07.n
!   s[0] := CHR(n);
LDGW tChrStr07.n
STGC tChrStr07.s
!   p(s);
GLOBAL tChrStr07.s
GLOBAL tChrStr07.p
CALL 1
!   n := ORD(t[0]);
LDGC tChrStr07.t
STGW tChrStr07.n
!   n := ORD(CHR(n+257))-n;
LDGW tChrStr07.n
CONST 257
PLUS
CONVNC
LDGW tChrStr07.n
MINUS
STGW tChrStr07.n
!   Out.Int(n, 0);
CONST 0
LDGW tChrStr07.n
GLOBAL Out.Int
CALL 2
!   Out.Ln
GLOBAL Out.Ln
CALL 0
RETURN
END

! Global variables
GLOVAR tChrStr07.n 4
GLOVAR tChrStr07.s 2
GLOVAR tChrStr07.t 3

! String "x"
DEFINE tChrStr07.%1
STRING 7800

! String "A"
DEFINE tChrStr07.%2
STRING 4100

! String "y"
DEFINE tChrStr07.%3
STRING 7900

! String "z"
DEFINE tChrStr07.%4
STRING 7A00

! End of file
]]*)
