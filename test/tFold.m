MODULE tFold;

(* Cases of constant folding for coverage *)

IMPORT Out;

BEGIN
  Out.Int(LSR(-1, 16), 0); Out.Ln;
  Out.Int(ORD(~FALSE), 0); Out.Ln;
  Out.Int(ORD(3 > 4) + ORD(3 >= 4) + ORD(3 # 4), 0); Out.Ln;
  Out.Int(ORD(TRUE OR FALSE), 0); Out.Ln;
  Out.Int(ORD({1,2,3} - {2}), 0); Out.Ln;
  Out.Real(2.0*3.0+4.0/5.0-1.0); Out.Ln;
END tFold.

(*<<
65535
1
1
1
10
5.80000
>>*)

(*[[
!! (SYMFILE #tFold STAMP #tFold.%main 1 #tFold.m)
!! (CHKSUM STAMP)
!! 
MODULE tFold STAMP 0
IMPORT Out STAMP
ENDHDR

PROC tFold.%main 0 3 0
!   Out.Int(LSR(-1, 16), 0); Out.Ln;
CONST 0
CONST 65535
GLOBAL Out.Int
CALL 2
GLOBAL Out.Ln
CALL 0
!   Out.Int(ORD(~FALSE), 0); Out.Ln;
CONST 0
CONST 1
GLOBAL Out.Int
CALL 2
GLOBAL Out.Ln
CALL 0
!   Out.Int(ORD(3 > 4) + ORD(3 >= 4) + ORD(3 # 4), 0); Out.Ln;
CONST 0
CONST 1
GLOBAL Out.Int
CALL 2
GLOBAL Out.Ln
CALL 0
!   Out.Int(ORD(TRUE OR FALSE), 0); Out.Ln;
CONST 0
CONST 1
GLOBAL Out.Int
CALL 2
GLOBAL Out.Ln
CALL 0
!   Out.Int(ORD({1,2,3} - {2}), 0); Out.Ln;
CONST 0
CONST 10
GLOBAL Out.Int
CALL 2
GLOBAL Out.Ln
CALL 0
!   Out.Real(2.0*3.0+4.0/5.0-1.0); Out.Ln;
FCONST 5.8
GLOBAL Out.Real
CALL 1
GLOBAL Out.Ln
CALL 0
RETURN
END

! End of file
]]*)
