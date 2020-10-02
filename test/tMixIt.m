MODULE tMixIt;

(*<<
6.14000
3.00000000000
6.14000034332
9.14000034332
12.1400003433
9.14000034332
9.14000034332
-3.00000
-3
-1.00000
-1.00000
>>*)

IMPORT Out;

VAR x: SHORTINT; y: INTEGER; r: REAL; s: LONGREAL;

BEGIN
  x := +3; r := 3.14;
  r := +r + x;
  Out.Real(r); Out.Ln;     (* 6.14000 *)

  s := x;
  Out.LongReal(s); Out.Ln; (* 3.00000000000 *)

  s := LONG(r);
  Out.LongReal(s); Out.Ln; (* 6.14000034332 *)

  s := s + 3;
  Out.LongReal(s); Out.Ln; (* 9.14000034332 *)

  s := s + x;
  Out.LongReal(s); Out.Ln; (* 12.1400003433 *)

  s := r + x;
  Out.LongReal(s); Out.Ln; (* 9.14000034332 *)

  s := LONG(r) + x;
  Out.LongReal(s); Out.Ln; (* 9.14000034332 *)

  x := -3; r := x; Out.Real(r); Out.Ln;
  x := SHORT(ENTIER(r)); Out.Int(x, 0); Out.Ln;
  y := 65535; r := SHORT(y); Out.Real(r); Out.Ln;
  r := SHORT(65535); Out.Real(r); Out.Ln;
END tMixIt.

(*[[
!! (SYMFILE #tMixIt STAMP #tMixIt.%main 1 #tMixIt.m)
!! (CHKSUM STAMP)
!! 
MODULE tMixIt STAMP 0
IMPORT Out STAMP
ENDHDR

PROC tMixIt.%main 0 4 0
!   x := +3; r := 3.14;
CONST 3
STGS tMixIt.x
FCONST 3.14
STGF tMixIt.r
!   r := +r + x;
LDGF tMixIt.r
LDGS tMixIt.x
CONVNF
FPLUS
STGF tMixIt.r
!   Out.Real(r); Out.Ln;     (* 6.14000 *)
LDGF tMixIt.r
GLOBAL Out.Real
CALL 1
GLOBAL Out.Ln
CALL 0
!   s := x;
LDGS tMixIt.x
CONVND
STGD tMixIt.s
!   Out.LongReal(s); Out.Ln; (* 3.00000000000 *)
LDGD tMixIt.s
GLOBAL Out.LongReal
CALL 2
GLOBAL Out.Ln
CALL 0
!   s := LONG(r);
LDGF tMixIt.r
CONVFD
STGD tMixIt.s
!   Out.LongReal(s); Out.Ln; (* 6.14000034332 *)
LDGD tMixIt.s
GLOBAL Out.LongReal
CALL 2
GLOBAL Out.Ln
CALL 0
!   s := s + 3;
LDGD tMixIt.s
DCONST 3.0
DPLUS
STGD tMixIt.s
!   Out.LongReal(s); Out.Ln; (* 9.14000034332 *)
LDGD tMixIt.s
GLOBAL Out.LongReal
CALL 2
GLOBAL Out.Ln
CALL 0
!   s := s + x;
LDGD tMixIt.s
LDGS tMixIt.x
CONVND
DPLUS
STGD tMixIt.s
!   Out.LongReal(s); Out.Ln; (* 12.1400003433 *)
LDGD tMixIt.s
GLOBAL Out.LongReal
CALL 2
GLOBAL Out.Ln
CALL 0
!   s := r + x;
LDGF tMixIt.r
LDGS tMixIt.x
CONVNF
FPLUS
CONVFD
STGD tMixIt.s
!   Out.LongReal(s); Out.Ln; (* 9.14000034332 *)
LDGD tMixIt.s
GLOBAL Out.LongReal
CALL 2
GLOBAL Out.Ln
CALL 0
!   s := LONG(r) + x;
LDGF tMixIt.r
CONVFD
LDGS tMixIt.x
CONVND
DPLUS
STGD tMixIt.s
!   Out.LongReal(s); Out.Ln; (* 9.14000034332 *)
LDGD tMixIt.s
GLOBAL Out.LongReal
CALL 2
GLOBAL Out.Ln
CALL 0
!   x := -3; r := x; Out.Real(r); Out.Ln;
CONST -3
STGS tMixIt.x
LDGS tMixIt.x
CONVNF
STGF tMixIt.r
LDGF tMixIt.r
GLOBAL Out.Real
CALL 1
GLOBAL Out.Ln
CALL 0
!   x := SHORT(ENTIER(r)); Out.Int(x, 0); Out.Ln;
LDGF tMixIt.r
CONVFN
STGS tMixIt.x
CONST 0
LDGS tMixIt.x
GLOBAL Out.Int
CALL 2
GLOBAL Out.Ln
CALL 0
!   y := 65535; r := SHORT(y); Out.Real(r); Out.Ln;
CONST 65535
STGW tMixIt.y
LDGW tMixIt.y
CONVNS
CONVNF
STGF tMixIt.r
LDGF tMixIt.r
GLOBAL Out.Real
CALL 1
GLOBAL Out.Ln
CALL 0
!   r := SHORT(65535); Out.Real(r); Out.Ln;
FCONST -1.0
STGF tMixIt.r
LDGF tMixIt.r
GLOBAL Out.Real
CALL 1
GLOBAL Out.Ln
CALL 0
RETURN
END

! Global variables
GLOVAR tMixIt.x 2
GLOVAR tMixIt.y 4
GLOVAR tMixIt.r 4
GLOVAR tMixIt.s 8

! End of file
]]*)
