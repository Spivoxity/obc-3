MODULE tStif;

(* Bug from Rob King: STIF not implemented *)

IMPORT Out;

VAR i: INTEGER; a: ARRAY 10 OF REAL;

BEGIN
  i := 3;
  a[i] := 4.5;
  Out.Real(a[3]);
  Out.Ln
END tStif.

(*<<
4.50000
>>*)

(*[[
!! (SYMFILE #tStif STAMP #tStif.%main 1 #tStif.m)
!! (CHKSUM STAMP)
!! 
MODULE tStif STAMP 0
IMPORT Out STAMP
ENDHDR

PROC tStif.%main 0 4 0
!   i := 3;
CONST 3
STGW tStif.i
!   a[i] := 4.5;
FCONST 4.5
GLOBAL tStif.a
LDGW tStif.i
CONST 10
BOUND 11
STIF
!   Out.Real(a[3]);
GLOBAL tStif.a
CONST 3
LDIF
GLOBAL Out.Real
CALL 1
!   Out.Ln
GLOBAL Out.Ln
CALL 0
RETURN
END

! Global variables
GLOVAR tStif.i 4
GLOVAR tStif.a 40

! End of file
]]*)
