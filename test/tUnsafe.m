MODULE tUnsafe;

IMPORT Out, SYSTEM;

VAR x: LONGINT; y: LONGREAL;
 u: INTEGER; v: REAL;

BEGIN
  y := 3.14;
  x := SYSTEM.VAL(LONGINT, y);
  Out.LongInt(x, 0); Out.Ln;
  x := x + 3000000;
  y := SYSTEM.VAL(LONGREAL, x-3000000) + 0.1;
  Out.LongReal(y); Out.Ln;
  x := SYSTEM.VAL(LONGINT, y-0.1);
  Out.LongInt(x, 0); Out.Ln;

  v := 3.14;
  u := SYSTEM.VAL(INTEGER, v);
  Out.Int(u, 0); Out.Ln;
  u := u + 3000000;
  v := SYSTEM.VAL(REAL, u-3000000) + 0.1;
  Out.Real(v); Out.Ln;
  u := SYSTEM.VAL(INTEGER, v-0.1);
  Out.Int(u, 0); Out.Ln
END tUnsafe.

(*<<
4614253070214989087
3.24000000000
4614253070214989087
1078523331
3.24000
1078523331
>>*)

(*[[
!! (SYMFILE #tUnsafe STAMP #tUnsafe.%main 1 #tUnsafe.m)
!! (CHKSUM STAMP)
!! 
MODULE tUnsafe STAMP 0
IMPORT Out STAMP
ENDHDR

PROC tUnsafe.%main 0 4 0
!   y := 3.14;
DCONST 3.14
STGD tUnsafe.y
!   x := SYSTEM.VAL(LONGINT, y);
LDGD tUnsafe.y
STGQ tUnsafe.x
!   Out.LongInt(x, 0); Out.Ln;
CONST 0
LDGQ tUnsafe.x
GLOBAL Out.LongInt
CALL 3
GLOBAL Out.Ln
CALL 0
!   x := x + 3000000;
LDGQ tUnsafe.x
QCONST 3000000
QPLUS
STGQ tUnsafe.x
!   y := SYSTEM.VAL(LONGREAL, x-3000000) + 0.1;
LDGQ tUnsafe.x
QCONST 3000000
QMINUS
DCONST 0.1
DPLUS
STGD tUnsafe.y
!   Out.LongReal(y); Out.Ln;
LDGD tUnsafe.y
GLOBAL Out.LongReal
CALL 2
GLOBAL Out.Ln
CALL 0
!   x := SYSTEM.VAL(LONGINT, y-0.1);
LDGD tUnsafe.y
DCONST 0.1
DMINUS
STGQ tUnsafe.x
!   Out.LongInt(x, 0); Out.Ln;
CONST 0
LDGQ tUnsafe.x
GLOBAL Out.LongInt
CALL 3
GLOBAL Out.Ln
CALL 0
!   v := 3.14;
FCONST 3.14
STGF tUnsafe.v
!   u := SYSTEM.VAL(INTEGER, v);
LDGF tUnsafe.v
STGW tUnsafe.u
!   Out.Int(u, 0); Out.Ln;
CONST 0
LDGW tUnsafe.u
GLOBAL Out.Int
CALL 2
GLOBAL Out.Ln
CALL 0
!   u := u + 3000000;
LDGW tUnsafe.u
CONST 3000000
PLUS
STGW tUnsafe.u
!   v := SYSTEM.VAL(REAL, u-3000000) + 0.1;
LDGW tUnsafe.u
CONST 3000000
MINUS
FCONST 0.1
FPLUS
STGF tUnsafe.v
!   Out.Real(v); Out.Ln;
LDGF tUnsafe.v
GLOBAL Out.Real
CALL 1
GLOBAL Out.Ln
CALL 0
!   u := SYSTEM.VAL(INTEGER, v-0.1);
LDGF tUnsafe.v
FCONST 0.1
FMINUS
STGW tUnsafe.u
!   Out.Int(u, 0); Out.Ln
CONST 0
LDGW tUnsafe.u
GLOBAL Out.Int
CALL 2
GLOBAL Out.Ln
CALL 0
RETURN
END

! Global variables
GLOVAR tUnsafe.x 8
GLOVAR tUnsafe.y 8
GLOVAR tUnsafe.u 4
GLOVAR tUnsafe.v 4

! End of file
]]*)
