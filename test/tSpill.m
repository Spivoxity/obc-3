MODULE tSpill;

IMPORT Out;

PROCEDURE Q(a, b, c, d, e, f, k, l, m, n, o, p: INTEGER);
BEGIN
  Out.Int(a, 0); Out.Ln;
  Out.Int(b, 0); Out.Ln;
  Out.Int(c, 0); Out.Ln;
  Out.Int(d, 0); Out.Ln;
  Out.Int(e, 0); Out.Ln;
  Out.Int(f, 0); Out.Ln;
  Out.Int(k, 0); Out.Ln;
  Out.Int(l, 0); Out.Ln;
  Out.Int(m, 0); Out.Ln;
  Out.Int(n, 0); Out.Ln;
  Out.Int(o, 0); Out.Ln;
  Out.Int(p, 0); Out.Ln
END Q;

PROCEDURE R(VAR u, v, w, x, y, z: INTEGER);
BEGIN
  Q(u, v, w, x, y, z, u, v, w, x, y, z)
END R;

VAR p, q, r: INTEGER;

BEGIN
  p := 3; q := 1; r := 2;
  R(p, q, r, p, q, r)
END tSpill.

(*<<
3
1
2
3
1
2
3
1
2
3
1
2
>>*)

(*[[
!! (SYMFILE #tSpill STAMP #tSpill.%main 1 #tSpill.m)
!! (CHKSUM STAMP)
!! 
MODULE tSpill STAMP 0
IMPORT Out STAMP
ENDHDR

PROC tSpill.Q 0 3 0
! PROCEDURE Q(a, b, c, d, e, f, k, l, m, n, o, p: INTEGER);
!   Out.Int(a, 0); Out.Ln;
CONST 0
LDLW 12
GLOBAL Out.Int
CALL 2
GLOBAL Out.Ln
CALL 0
!   Out.Int(b, 0); Out.Ln;
CONST 0
LDLW 16
GLOBAL Out.Int
CALL 2
GLOBAL Out.Ln
CALL 0
!   Out.Int(c, 0); Out.Ln;
CONST 0
LDLW 20
GLOBAL Out.Int
CALL 2
GLOBAL Out.Ln
CALL 0
!   Out.Int(d, 0); Out.Ln;
CONST 0
LDLW 24
GLOBAL Out.Int
CALL 2
GLOBAL Out.Ln
CALL 0
!   Out.Int(e, 0); Out.Ln;
CONST 0
LDLW 28
GLOBAL Out.Int
CALL 2
GLOBAL Out.Ln
CALL 0
!   Out.Int(f, 0); Out.Ln;
CONST 0
LDLW 32
GLOBAL Out.Int
CALL 2
GLOBAL Out.Ln
CALL 0
!   Out.Int(k, 0); Out.Ln;
CONST 0
LDLW 36
GLOBAL Out.Int
CALL 2
GLOBAL Out.Ln
CALL 0
!   Out.Int(l, 0); Out.Ln;
CONST 0
LDLW 40
GLOBAL Out.Int
CALL 2
GLOBAL Out.Ln
CALL 0
!   Out.Int(m, 0); Out.Ln;
CONST 0
LDLW 44
GLOBAL Out.Int
CALL 2
GLOBAL Out.Ln
CALL 0
!   Out.Int(n, 0); Out.Ln;
CONST 0
LDLW 48
GLOBAL Out.Int
CALL 2
GLOBAL Out.Ln
CALL 0
!   Out.Int(o, 0); Out.Ln;
CONST 0
LDLW 52
GLOBAL Out.Int
CALL 2
GLOBAL Out.Ln
CALL 0
!   Out.Int(p, 0); Out.Ln
CONST 0
LDLW 56
GLOBAL Out.Int
CALL 2
GLOBAL Out.Ln
CALL 0
RETURN
END

PROC tSpill.R 0 13 0x03f00001
! PROCEDURE R(VAR u, v, w, x, y, z: INTEGER);
!   Q(u, v, w, x, y, z, u, v, w, x, y, z)
LDLW 32
LOADW
LDLW 28
LOADW
LDLW 24
LOADW
LDLW 20
LOADW
LDLW 16
LOADW
LDLW 12
LOADW
LDLW 32
LOADW
LDLW 28
LOADW
LDLW 24
LOADW
LDLW 20
LOADW
LDLW 16
LOADW
LDLW 12
LOADW
GLOBAL tSpill.Q
CALL 12
RETURN
END

PROC tSpill.%main 0 7 0
!   p := 3; q := 1; r := 2;
CONST 3
STGW tSpill.p
CONST 1
STGW tSpill.q
CONST 2
STGW tSpill.r
!   R(p, q, r, p, q, r)
GLOBAL tSpill.r
GLOBAL tSpill.q
GLOBAL tSpill.p
GLOBAL tSpill.r
GLOBAL tSpill.q
GLOBAL tSpill.p
GLOBAL tSpill.R
CALL 6
RETURN
END

! Global variables
GLOVAR tSpill.p 4
GLOVAR tSpill.q 4
GLOVAR tSpill.r 4

! End of file
]]*)
