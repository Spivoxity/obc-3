MODULE tParamGC;

(* The local copies of aggregate value parameters must be included in
   the GC map of a procedure. *)

IMPORT Out, GC;

TYPE ptr = POINTER TO blob; blob = RECORD val: INTEGER; lnk: ptr END;

TYPE arr = ARRAY 2 OF ptr;

VAR x: ptr; b: arr;

PROCEDURE P(a: arr);
BEGIN
  GC.Collect;
  x.val := 3;
  Out.Int(a[0].val, 0); Out.Ln  
END P;

BEGIN
  NEW(x); NEW(x);
  b[0] := x;
  x.val := 2;
  P(b)
END tParamGC.

(*<<
3
>>*)

(*[[
!! SYMFILE #tParamGC STAMP #tParamGC.%main 1
!! END STAMP
!! 
MODULE tParamGC STAMP 0
IMPORT Out STAMP
IMPORT GC STAMP
ENDHDR

PROC tParamGC.P 2 4 0x00018001
! PROCEDURE P(a: arr);
LOCAL -8
LDLW 12
CONST 8
FIXCOPY
!   GC.Collect;
GLOBAL GC.Collect
CALL 0
!   x.val := 3;
CONST 3
LDGW tParamGC.x
NCHECK 17
STOREW
!   Out.Int(a[0].val, 0); Out.Ln  
CONST 0
LDLW -8
NCHECK 18
LOADW
GLOBAL Out.Int
CALL 2
GLOBAL Out.Ln
CALL 0
RETURN
END

PROC tParamGC.%main 0 4 0
!   NEW(x); NEW(x);
CONST 8
GLOBAL tParamGC.blob
GLOBAL tParamGC.x
GLOBAL NEW
CALL 3
CONST 8
GLOBAL tParamGC.blob
GLOBAL tParamGC.x
GLOBAL NEW
CALL 3
!   b[0] := x;
LDGW tParamGC.x
STGW tParamGC.b
!   x.val := 2;
CONST 2
LDGW tParamGC.x
NCHECK 24
STOREW
!   P(b)
GLOBAL tParamGC.b
GLOBAL tParamGC.P
CALL 1
RETURN
END

! Global variables
GLOVAR tParamGC.x 4
GLOVAR tParamGC.b 8

! Pointer map
DEFINE tParamGC.%gcmap
WORD GC_BASE
WORD tParamGC.x
WORD 0
WORD GC_BASE
WORD tParamGC.b
WORD GC_MAP
WORD 0x00000007
WORD GC_END

! Descriptor for blob
DEFINE tParamGC.blob
WORD 0x00000005
WORD 0
WORD tParamGC.blob.%anc

DEFINE tParamGC.blob.%anc
WORD tParamGC.blob

! End of file
]]*)
