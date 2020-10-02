MODULE tStif07;

(* Bug from Rob King: STIF not implemented *)

IMPORT Out;

VAR i: INTEGER; a: ARRAY 10 OF REAL;

BEGIN
  i := 3;
  a[i] := 4.5;
  Out.Real(a[3]);
  Out.Ln
END tStif07.

(*<<
4.50000
>>*)

(*[[
!! (SYMFILE #tStif07 STAMP #tStif07.%main 1 #tStif07.m)
!! (CHKSUM STAMP)
!! 
MODULE tStif07 STAMP 0
IMPORT Out STAMP
ENDHDR

PROC tStif07.%main 0 4 0
!   i := 3;
CONST 3
STGW tStif07.i
!   a[i] := 4.5;
FCONST 4.5
GLOBAL tStif07.a
LDGW tStif07.i
CONST 10
BOUND 11
STIF
!   Out.Real(a[3]);
GLOBAL tStif07.a
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
GLOVAR tStif07.i 4
GLOVAR tStif07.a 40

! End of file
]]*)
