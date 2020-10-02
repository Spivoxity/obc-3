MODULE tDynAssign;

(* Check for dynamic type test on assigning to a 
   VAR parameter of record type *)

TYPE R = RECORD x: INTEGER END;

TYPE S = RECORD (R) y: INTEGER END;

PROCEDURE (VAR r: R) Assign;
  VAR r2: R;
BEGIN
  r2.x := 3;
  r := r2
END Assign;

PROCEDURE Assign2(VAR r: R);
  VAR r2: R;
BEGIN
  r2.x := 3;
  r := r2
END Assign2;

VAR s: S;

BEGIN
  s.Assign;
  Assign2(s)
END tDynAssign.

(*<<
Runtime error: dynamic type error in record assignment on line 14 in module tDynAssign
In procedure tDynAssign.R.Assign
   called from tDynAssign.%main
   called from MAIN
>>*)

(*[[
!! (SYMFILE #tDynAssign STAMP #tDynAssign.%main 1 #tDynAssign.m)
!! (CHKSUM STAMP)
!! 
MODULE tDynAssign STAMP 0
ENDHDR

PROC tDynAssign.R.Assign 4 3 0x00100001
! PROCEDURE (VAR r: R) Assign;
!   r2.x := 3;
CONST 3
STLW -4
!   r := r2
LDLW 16
GLOBAL tDynAssign.R
JEQ L1
ERROR E_ASSIGN 14
LABEL L1
LDLW 12
LOCAL -4
CONST 4
FIXCOPY
RETURN
END

PROC tDynAssign.Assign2 4 3 0x00100001
! PROCEDURE Assign2(VAR r: R);
!   r2.x := 3;
CONST 3
STLW -4
!   r := r2
LDLW 16
GLOBAL tDynAssign.R
JEQ L2
ERROR E_ASSIGN 21
LABEL L2
LDLW 12
LOCAL -4
CONST 4
FIXCOPY
RETURN
END

PROC tDynAssign.%main 0 3 0
!   s.Assign;
GLOBAL tDynAssign.S
GLOBAL tDynAssign.s
GLOBAL tDynAssign.R.Assign
CALL 2
!   Assign2(s)
GLOBAL tDynAssign.S
GLOBAL tDynAssign.s
GLOBAL tDynAssign.Assign2
CALL 2
RETURN
END

! Global variables
GLOVAR tDynAssign.s 8

! Descriptor for R
DEFINE tDynAssign.R
WORD 0
WORD 0
WORD tDynAssign.R.%anc
WORD tDynAssign.R.Assign

DEFINE tDynAssign.R.%anc
WORD tDynAssign.R

! Descriptor for S
DEFINE tDynAssign.S
WORD 0
WORD 1
WORD tDynAssign.S.%anc
WORD tDynAssign.R.Assign

DEFINE tDynAssign.S.%anc
WORD tDynAssign.R
WORD tDynAssign.S

! End of file
]]*)
