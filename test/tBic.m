MODULE tBic;

(* Generates a BIC instruction on the ARM *)

IMPORT Out, SYSTEM;

VAR s: SET;

BEGIN
  s := { 0 .. 15 };
  s := s * (- { 0 .. 3 });
  Out.Int(SYSTEM.VAL(INTEGER, s), 0);
  Out.Ln
END tBic.

(*<<
65520
>>*)

(*[[
!! (SYMFILE #tBic STAMP #tBic.%main 1 #tBic.m)
!! (CHKSUM STAMP)
!! 
MODULE tBic STAMP 0
IMPORT Out STAMP
ENDHDR

PROC tBic.%main 0 3 0
!   s := { 0 .. 15 };
CONST 65535
STGW tBic.s
!   s := s * (- { 0 .. 3 });
LDGW tBic.s
CONST -16
BITAND
STGW tBic.s
!   Out.Int(SYSTEM.VAL(INTEGER, s), 0);
CONST 0
LDGW tBic.s
GLOBAL Out.Int
CALL 2
!   Out.Ln
GLOBAL Out.Ln
CALL 0
RETURN
END

! Global variables
GLOVAR tBic.s 4

! End of file
]]*)
