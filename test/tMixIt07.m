MODULE tMixIt07;

(*<<
6.14000
3.00000000000
6.14000034332
9.14000034332
12.1400003433
9.14000034332
-3.00000
-3
65535.0
65535.0
>>*)

IMPORT Out;

VAR x: SHORTINT; y: INTEGER; r: REAL; s: LONGREAL;

BEGIN
  x := +3; r := 3.14;
  r := +r + FLT(x);
  Out.Real(r); Out.Ln;     (* 6.14000 *)

  s := FLT(x);
  Out.LongReal(s); Out.Ln; (* 3.00000000000 *)

  s := r;
  Out.LongReal(s); Out.Ln; (* 6.14000034332 *)

  s := s + FLT(3);
  Out.LongReal(s); Out.Ln; (* 9.14000034332 *)

  s := s + FLT(x);
  Out.LongReal(s); Out.Ln; (* 12.1400003433 *)

  s := r + FLT(x);
  Out.LongReal(s); Out.Ln; (* 9.14000034332 *)

  x := -3; r := FLT(x); Out.Real(r); Out.Ln;
  x := FLOOR(r); Out.Int(x, 0); Out.Ln;
  y := 65535; r := FLT(y); Out.Real(r); Out.Ln;
  r := FLT(65535); Out.Real(r); Out.Ln;
END tMixIt07.

(*[[
!! (SYMFILE #tMixIt07 STAMP #tMixIt07.%main 1 #tMixIt07.m)
!! (CHKSUM STAMP)
!! 
MODULE tMixIt07 STAMP 0
IMPORT Out STAMP
ENDHDR

PROC tMixIt07.%main 0 4 0
!   x := +3; r := 3.14;
CONST 3
STGS tMixIt07.x
FCONST 3.14
STGF tMixIt07.r
!   r := +r + FLT(x);
LDGF tMixIt07.r
LDGS tMixIt07.x
CONVNF
FPLUS
STGF tMixIt07.r
!   Out.Real(r); Out.Ln;     (* 6.14000 *)
LDGF tMixIt07.r
GLOBAL Out.Real
CALL 1
GLOBAL Out.Ln
CALL 0
!   s := FLT(x);
LDGS tMixIt07.x
CONVND
STGD tMixIt07.s
!   Out.LongReal(s); Out.Ln; (* 3.00000000000 *)
LDGD tMixIt07.s
GLOBAL Out.LongReal
CALL 2
GLOBAL Out.Ln
CALL 0
!   s := r;
LDGF tMixIt07.r
CONVFD
STGD tMixIt07.s
!   Out.LongReal(s); Out.Ln; (* 6.14000034332 *)
LDGD tMixIt07.s
GLOBAL Out.LongReal
CALL 2
GLOBAL Out.Ln
CALL 0
!   s := s + FLT(3);
LDGD tMixIt07.s
DCONST 3.0
DPLUS
STGD tMixIt07.s
!   Out.LongReal(s); Out.Ln; (* 9.14000034332 *)
LDGD tMixIt07.s
GLOBAL Out.LongReal
CALL 2
GLOBAL Out.Ln
CALL 0
!   s := s + FLT(x);
LDGD tMixIt07.s
LDGS tMixIt07.x
CONVND
DPLUS
STGD tMixIt07.s
!   Out.LongReal(s); Out.Ln; (* 12.1400003433 *)
LDGD tMixIt07.s
GLOBAL Out.LongReal
CALL 2
GLOBAL Out.Ln
CALL 0
!   s := r + FLT(x);
LDGF tMixIt07.r
LDGS tMixIt07.x
CONVNF
FPLUS
CONVFD
STGD tMixIt07.s
!   Out.LongReal(s); Out.Ln; (* 9.14000034332 *)
LDGD tMixIt07.s
GLOBAL Out.LongReal
CALL 2
GLOBAL Out.Ln
CALL 0
!   x := -3; r := FLT(x); Out.Real(r); Out.Ln;
CONST -3
STGS tMixIt07.x
LDGS tMixIt07.x
CONVNF
STGF tMixIt07.r
LDGF tMixIt07.r
GLOBAL Out.Real
CALL 1
GLOBAL Out.Ln
CALL 0
!   x := FLOOR(r); Out.Int(x, 0); Out.Ln;
LDGF tMixIt07.r
CONVFN
STGS tMixIt07.x
CONST 0
LDGS tMixIt07.x
GLOBAL Out.Int
CALL 2
GLOBAL Out.Ln
CALL 0
!   y := 65535; r := FLT(y); Out.Real(r); Out.Ln;
CONST 65535
STGW tMixIt07.y
LDGW tMixIt07.y
CONVNF
STGF tMixIt07.r
LDGF tMixIt07.r
GLOBAL Out.Real
CALL 1
GLOBAL Out.Ln
CALL 0
!   r := FLT(65535); Out.Real(r); Out.Ln;
FCONST 65535.0
STGF tMixIt07.r
LDGF tMixIt07.r
GLOBAL Out.Real
CALL 1
GLOBAL Out.Ln
CALL 0
RETURN
END

! Global variables
GLOVAR tMixIt07.x 2
GLOVAR tMixIt07.y 4
GLOVAR tMixIt07.r 4
GLOVAR tMixIt07.s 8

! End of file
]]*)
