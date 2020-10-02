MODULE tEgyptian;

IMPORT Out;

PROCEDURE Egyptian(a, b: INTEGER; VAR d: ARRAY OF LONGINT): INTEGER;
  VAR p, q, m: LONGINT; n: INTEGER;
BEGIN
  p := a; q := b; n := 0;
  WHILE p > 0 DO
    m := (q-1) DIV p + 1;
    (* p*(m-1) < q <= p*m, so p/q = 1/m + (pm - q)/qm
       with 0 <= pm - q < p *)
    d[n] := m; n := n+1;
    p := p*m - q; q := q*m
  END;
  RETURN n
END Egyptian;

VAR 
  digits: ARRAY 100 OF LONGINT;
  a, b, n, i: INTEGER;

BEGIN
  a := 144; b := 233;
  n := Egyptian(a, b, digits);

  Out.Int(a,0); Out.Char('/'); Out.Int(b,0);
  Out.String(" = ");
  FOR i := 0 TO n-1 DO
    IF i > 0 THEN Out.String(" + ") END;
    Out.String("1/"); Out.LongInt(digits[i], 0);
  END;
  Out.Ln
END tEgyptian.

(*<<
144/233 = 1/2 + 1/9 + 1/145 + 1/55285 + 1/6724093410
>>*)

(*[[
!! (SYMFILE #tEgyptian STAMP #tEgyptian.%main 1 #tEgyptian.m)
!! (CHKSUM STAMP)
!! 
MODULE tEgyptian STAMP 0
IMPORT Out STAMP
ENDHDR

PROC tEgyptian.Egyptian 28 5 0x00400001
! PROCEDURE Egyptian(a, b: INTEGER; VAR d: ARRAY OF LONGINT): INTEGER;
!   p := a; q := b; n := 0;
LDLW 12
CONVNQ
STLQ -8
LDLW 16
CONVNQ
STLQ -16
CONST 0
STLW -28
LABEL L4
!   WHILE p > 0 DO
LDLQ -8
CONST 0
CONVNQ
QJLEQ L6
!     m := (q-1) DIV p + 1;
LDLQ -16
CONST 1
CONVNQ
QMINUS
LDLQ -8
QZCHECK 10
QDIV
CONST 1
CONVNQ
QPLUS
STLQ -24
!     d[n] := m; n := n+1;
LDLQ -24
LDLW 20
LDLW -28
LDLW 24
BOUND 13
STIQ
INCL -28
!     p := p*m - q; q := q*m
LDLQ -8
LDLQ -24
QTIMES
LDLQ -16
QMINUS
STLQ -8
LDLQ -16
LDLQ -24
QTIMES
STLQ -16
JUMP L4
LABEL L6
!   RETURN n
LDLW -28
RETURN
END

PROC tEgyptian.%main 4 5 0
!   a := 144; b := 233;
CONST 144
STGW tEgyptian.a
CONST 233
STGW tEgyptian.b
!   n := Egyptian(a, b, digits);
CONST 100
GLOBAL tEgyptian.digits
LDGW tEgyptian.b
LDGW tEgyptian.a
GLOBAL tEgyptian.Egyptian
CALLW 4
STGW tEgyptian.n
!   Out.Int(a,0); Out.Char('/'); Out.Int(b,0);
CONST 0
LDGW tEgyptian.a
GLOBAL Out.Int
CALL 2
CONST 47
ALIGNC
GLOBAL Out.Char
CALL 1
CONST 0
LDGW tEgyptian.b
GLOBAL Out.Int
CALL 2
!   Out.String(" = ");
CONST 4
GLOBAL tEgyptian.%1
GLOBAL Out.String
CALL 2
!   FOR i := 0 TO n-1 DO
LDGW tEgyptian.n
DEC
STLW -4
CONST 0
STGW tEgyptian.i
LABEL L7
LDGW tEgyptian.i
LDLW -4
JGT L8
!     IF i > 0 THEN Out.String(" + ") END;
LDGW tEgyptian.i
JLEQZ L11
CONST 4
GLOBAL tEgyptian.%2
GLOBAL Out.String
CALL 2
LABEL L11
!     Out.String("1/"); Out.LongInt(digits[i], 0);
CONST 3
GLOBAL tEgyptian.%3
GLOBAL Out.String
CALL 2
CONST 0
GLOBAL tEgyptian.digits
LDGW tEgyptian.i
CONST 100
BOUND 31
LDIQ
GLOBAL Out.LongInt
CALL 3
!   FOR i := 0 TO n-1 DO
LDGW tEgyptian.i
INC
STGW tEgyptian.i
JUMP L7
LABEL L8
!   Out.Ln
GLOBAL Out.Ln
CALL 0
RETURN
END

! Global variables
GLOVAR tEgyptian.digits 800
GLOVAR tEgyptian.a 4
GLOVAR tEgyptian.b 4
GLOVAR tEgyptian.n 4
GLOVAR tEgyptian.i 4

! String " = "
DEFINE tEgyptian.%1
STRING 203D2000

! String " + "
DEFINE tEgyptian.%2
STRING 202B2000

! String "1/"
DEFINE tEgyptian.%3
STRING 312F00

! End of file
]]*)
