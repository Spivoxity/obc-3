MODULE tPeep;

(* Test coverage for peephole optimiser *)

VAR x: REAL; k: INTEGER; b: BOOLEAN; a: ARRAY 10 OF INTEGER;
  
BEGIN
  x := x / 2.0;

  CASE 5 OF
    MIN(INTEGER)..4: x := x + 0.5
  END;

  k := k + 3 + 4;

  b := ~(x = 0.0);

  a[2] := k
END tPeep.

(*<<
Runtime error: no matching label in CASE statement on line 10 in module tPeep
In procedure tPeep.%main
   called from MAIN
>>*)

(*[[
!! (SYMFILE #tPeep STAMP #tPeep.%main 1 #tPeep.m)
!! (CHKSUM STAMP)
!! 
MODULE tPeep STAMP 0
ENDHDR

PROC tPeep.%main 0 4 0
!   x := x / 2.0;
LDGF tPeep.x
FCONST 2.0
FDIV
STGF tPeep.x
ERROR E_CASE 10
!   k := k + 3 + 4;
LDGW tPeep.k
CONST 7
PLUS
STGW tPeep.k
!   b := ~(x = 0.0);
LDGF tPeep.x
FCONST 0.0
FNEQ
STGC tPeep.b
!   a[2] := k
LDGW tPeep.k
GLOBAL tPeep.a
STNW 8
RETURN
END

! Global variables
GLOVAR tPeep.x 4
GLOVAR tPeep.k 4
GLOVAR tPeep.b 1
GLOVAR tPeep.a 40

! End of file
]]*)
