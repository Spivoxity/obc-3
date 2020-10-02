MODULE tConv07;

(* Peepopt combines CONVNF and CONVFD to avoid loss of precision *)

IMPORT Out;

VAR x: LONGREAL; n: INTEGER;

BEGIN
  n := 1234567890;
  x := FLT(n);
  Out.LongReal(x); Out.Ln;

  x := FLT(1234567890);
  Out.LongReal(x); Out.Ln;
END tConv07.

(*<<
1234567890.00
1234567890.00
>>*)

(*[[
!! (SYMFILE #tConv07 STAMP #tConv07.%main 1 #tConv07.m)
!! (CHKSUM STAMP)
!! 
MODULE tConv07 STAMP 0
IMPORT Out STAMP
ENDHDR

PROC tConv07.%main 0 3 0
!   n := 1234567890;
CONST 1234567890
STGW tConv07.n
!   x := FLT(n);
LDGW tConv07.n
CONVND
STGD tConv07.x
!   Out.LongReal(x); Out.Ln;
LDGD tConv07.x
GLOBAL Out.LongReal
CALL 2
GLOBAL Out.Ln
CALL 0
!   x := FLT(1234567890);
DCONST 1234567890.0
STGD tConv07.x
!   Out.LongReal(x); Out.Ln;
LDGD tConv07.x
GLOBAL Out.LongReal
CALL 2
GLOBAL Out.Ln
CALL 0
RETURN
END

! Global variables
GLOVAR tConv07.x 8
GLOVAR tConv07.n 4

! End of file
]]*)
