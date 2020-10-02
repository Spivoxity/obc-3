MODULE tKleinert;

IMPORT Out;

VAR A: POINTER TO ARRAY OF INTEGER;

(* Bug in JIT: the open array parameter to Generate causes a base register
   to be allocated; the first call to MOD confuses the register allocator
   so that the base register is no longer reserved; the assignment
   A[0] := A[i] generates sufficient register pressure that the base
   register is trashed, and the subsequent second call to MOD segfaults. *)

PROCEDURE Generate(iA: ARRAY OF INTEGER);
VAR i, tmp: INTEGER;
BEGIN
  NEW(A, 4);
  i := 2;
  tmp := i MOD 2;
  A[0] := A[i];
  tmp := i MOD 2;
END Generate;

VAR tstArr: ARRAY 4 OF INTEGER;

BEGIN
  Generate(tstArr);
  Out.String("OK"); Out.Ln
END tKleinert.

(*<<
OK
>>*)

(*[[
!! (SYMFILE #tKleinert STAMP #tKleinert.%main 1 #tKleinert.m)
!! (CHKSUM STAMP)
!! 
MODULE tKleinert STAMP 0
IMPORT Out STAMP
ENDHDR

PROC tKleinert.Generate 8 5 0
! PROCEDURE Generate(iA: ARRAY OF INTEGER);
LOCAL 12
LDLW 16
CONST 4
TIMES
FLEXCOPY
!   NEW(A, 4);
CONST 4
CONST 1
CONST 4
CONST 0
GLOBAL NEWFLEX
CALLW 4
STGW tKleinert.A
!   i := 2;
CONST 2
STLW -4
!   tmp := i MOD 2;
LDLW -4
CONST 2
MOD
STLW -8
!   A[0] := A[i];
LDGW tKleinert.A
NCHECK 19
LDLW -4
DUP 1
LDNW -4
LDNW 4
BOUND 19
LDIW
LDGW tKleinert.A
NCHECK 19
CONST 0
DUP 1
LDNW -4
LDNW 4
BOUND 19
STIW
!   tmp := i MOD 2;
LDLW -4
CONST 2
MOD
STLW -8
RETURN
END

PROC tKleinert.%main 0 3 0
!   Generate(tstArr);
CONST 4
GLOBAL tKleinert.tstArr
GLOBAL tKleinert.Generate
CALL 2
!   Out.String("OK"); Out.Ln
CONST 3
GLOBAL tKleinert.%1
GLOBAL Out.String
CALL 2
GLOBAL Out.Ln
CALL 0
RETURN
END

! Global variables
GLOVAR tKleinert.A 4
GLOVAR tKleinert.tstArr 16

! Global pointer map
DEFINE tKleinert.%gcmap
WORD GC_POINTER
WORD tKleinert.A
WORD GC_END

! String "OK"
DEFINE tKleinert.%1
STRING 4F4B00

! End of file
]]*)
