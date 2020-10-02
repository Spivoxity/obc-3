MODULE tDynAssign07;

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
END tDynAssign07.

(*<<
Runtime error: dynamic type error in record assignment on line 14 in module tDynAssign07
In procedure tDynAssign07.R.Assign
   called from tDynAssign07.%main
   called from MAIN
>>*)

(*[[
!! (SYMFILE #tDynAssign07 STAMP #tDynAssign07.%main 1 #tDynAssign07.m)
!! (CHKSUM STAMP)
!! 
MODULE tDynAssign07 STAMP 0
ENDHDR

PROC tDynAssign07.R.Assign 4 3 0x00100001
! PROCEDURE (VAR r: R) Assign;
!   r2.x := 3;
CONST 3
STLW -4
!   r := r2
LDLW 16
GLOBAL tDynAssign07.R
JEQ L1
ERROR E_ASSIGN 14
LABEL L1
LDLW 12
LOCAL -4
CONST 4
FIXCOPY
RETURN
END

PROC tDynAssign07.Assign2 4 3 0x00100001
! PROCEDURE Assign2(VAR r: R);
!   r2.x := 3;
CONST 3
STLW -4
!   r := r2
LDLW 16
GLOBAL tDynAssign07.R
JEQ L2
ERROR E_ASSIGN 21
LABEL L2
LDLW 12
LOCAL -4
CONST 4
FIXCOPY
RETURN
END

PROC tDynAssign07.%main 0 3 0
!   s.Assign;
GLOBAL tDynAssign07.S
GLOBAL tDynAssign07.s
GLOBAL tDynAssign07.R.Assign
CALL 2
!   Assign2(s)
GLOBAL tDynAssign07.S
GLOBAL tDynAssign07.s
GLOBAL tDynAssign07.Assign2
CALL 2
RETURN
END

! Global variables
GLOVAR tDynAssign07.s 8

! Descriptor for R
DEFINE tDynAssign07.R
WORD 0
WORD 0
WORD tDynAssign07.R.%anc
WORD tDynAssign07.R.Assign

DEFINE tDynAssign07.R.%anc
WORD tDynAssign07.R

! Descriptor for S
DEFINE tDynAssign07.S
WORD 0
WORD 1
WORD tDynAssign07.S.%anc
WORD tDynAssign07.R.Assign

DEFINE tDynAssign07.S.%anc
WORD tDynAssign07.R
WORD tDynAssign07.S

! End of file
]]*)
