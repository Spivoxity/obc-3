MODULE tSwap;

IMPORT Out;

PROCEDURE one;
  VAR x, y, z: INTEGER;
BEGIN
  x, y := 3, 4;
  x, y := y, x;
  Out.Int(x, 0); Out.Int(y, 2); Out.Ln;
  x, y, z := 7, 8, 9;
  x, y, z := y, z, x;
  Out.Int(x, 0); Out.Int(y, 2); Out.Int(z, 2); Out.Ln;
END one;

PROCEDURE two;
  VAR i: INTEGER; a: ARRAY 4 OF INTEGER;
BEGIN
  i := 2;
  a[0] := 3; a[1] := 1; a[2] := 4; a[3] := 5;

  a[i], i := i, a[i];

  Out.Int(i, 0);
  Out.Int(a[0], 2); Out.Int(a[1], 2); Out.Int(a[2], 2); Out.Int(a[3], 2);
  Out.Ln
END two;

PROCEDURE three;
TYPE ptr = POINTER TO rec;
  rec = RECORD next: ptr; val: INTEGER END;

VAR p, q, r: ptr;

BEGIN
  NEW(p); NEW(q);
  p.next := q; p.val := 2;
  q.next := NIL; q.val := 3;
  r := p;

  r^.next, r := r, r^.next;

  Out.Int(p.val, 0);
  Out.Int(p.next.val, 2);
  Out.Int(q.val, 2);
  Out.Int(r.val, 2);
  Out.Ln
END three;

BEGIN
  one; two; three
END tSwap.

(*<<
4 3
8 9 7
4 3 1 2 5
2 2 3 3
>>*)

(*[[
!! (SYMFILE #tSwap STAMP #tSwap.%main 1 #tSwap.m)
!! (CHKSUM STAMP)
!! 
MODULE tSwap STAMP 0
IMPORT Out STAMP
ENDHDR

PROC tSwap.one 12 6 0
! PROCEDURE one;
!   x, y := 3, 4;
CONST 4
STLW -8
CONST 3
STLW -4
!   x, y := y, x;
LDLW -8
LDLW -4
STLW -8
STLW -4
!   Out.Int(x, 0); Out.Int(y, 2); Out.Ln;
CONST 0
LDLW -4
GLOBAL Out.Int
CALL 2
CONST 2
LDLW -8
GLOBAL Out.Int
CALL 2
GLOBAL Out.Ln
CALL 0
!   x, y, z := 7, 8, 9;
CONST 9
STLW -12
CONST 8
STLW -8
CONST 7
STLW -4
!   x, y, z := y, z, x;
LDLW -8
LOCAL -4
LDLW -12
LDLW -4
STLW -12
STLW -8
STOREW
!   Out.Int(x, 0); Out.Int(y, 2); Out.Int(z, 2); Out.Ln;
CONST 0
LDLW -4
GLOBAL Out.Int
CALL 2
CONST 2
LDLW -8
GLOBAL Out.Int
CALL 2
CONST 2
LDLW -12
GLOBAL Out.Int
CALL 2
GLOBAL Out.Ln
CALL 0
RETURN
END

PROC tSwap.two 20 5 0
! PROCEDURE two;
!   i := 2;
CONST 2
STLW -4
!   a[0] := 3; a[1] := 1; a[2] := 4; a[3] := 5;
CONST 3
STLW -20
CONST 1
STLW -16
CONST 4
STLW -12
CONST 5
STLW -8
!   a[i], i := i, a[i];
LDLW -4
LOCAL -20
LDLW -4
CONST 4
BOUND 22
INDEXW
LOCAL -20
LDLW -4
CONST 4
BOUND 22
LDIW
STLW -4
STOREW
!   Out.Int(i, 0);
CONST 0
LDLW -4
GLOBAL Out.Int
CALL 2
!   Out.Int(a[0], 2); Out.Int(a[1], 2); Out.Int(a[2], 2); Out.Int(a[3], 2);
CONST 2
LDLW -20
GLOBAL Out.Int
CALL 2
CONST 2
LDLW -16
GLOBAL Out.Int
CALL 2
CONST 2
LDLW -12
GLOBAL Out.Int
CALL 2
CONST 2
LDLW -8
GLOBAL Out.Int
CALL 2
!   Out.Ln
GLOBAL Out.Ln
CALL 0
RETURN
END

PROC tSwap.three 12 4 0x0001c001
! PROCEDURE three;
!   NEW(p); NEW(q);
CONST 8
GLOBAL tSwap.%1
GLOBAL NEW
CALLW 2
STLW -4
CONST 8
GLOBAL tSwap.%1
GLOBAL NEW
CALLW 2
STLW -8
!   p.next := q; p.val := 2;
LDLW -8
LDLW -4
NCHECK 37
STOREW
CONST 2
LDLW -4
NCHECK 37
STNW 4
!   q.next := NIL; q.val := 3;
CONST 0
LDLW -8
NCHECK 38
STOREW
CONST 3
LDLW -8
NCHECK 38
STNW 4
!   r := p;
LDLW -4
STLW -12
!   r^.next, r := r, r^.next;
LDLW -12
LDLW -12
NCHECK 41
LDLW -12
NCHECK 41
LOADW
STLW -12
STOREW
!   Out.Int(p.val, 0);
CONST 0
LDLW -4
NCHECK 43
LDNW 4
GLOBAL Out.Int
CALL 2
!   Out.Int(p.next.val, 2);
CONST 2
LDLW -4
NCHECK 44
LOADW
NCHECK 44
LDNW 4
GLOBAL Out.Int
CALL 2
!   Out.Int(q.val, 2);
CONST 2
LDLW -8
NCHECK 45
LDNW 4
GLOBAL Out.Int
CALL 2
!   Out.Int(r.val, 2);
CONST 2
LDLW -12
NCHECK 46
LDNW 4
GLOBAL Out.Int
CALL 2
!   Out.Ln
GLOBAL Out.Ln
CALL 0
RETURN
END

PROC tSwap.%main 0 1 0
!   one; two; three
GLOBAL tSwap.one
CALL 0
GLOBAL tSwap.two
CALL 0
GLOBAL tSwap.three
CALL 0
RETURN
END

! Descriptor for rec
DEFINE tSwap.%1
WORD 0x00000003
WORD 0
WORD tSwap.%1.%anc

DEFINE tSwap.%1.%anc
WORD tSwap.%1

! End of file
]]*)
