MODULE tParamGC07;

(* The local copies of aggregate value parameters must be included in
   the GC map of a procedure. *)

IMPORT Out, SYSTEM;

TYPE ptr = POINTER TO blob; blob = RECORD val: INTEGER; lnk: ptr END;

TYPE arr = ARRAY 2 OF ptr;

VAR x: ptr; b: arr;

PROCEDURE P(a: arr);
BEGIN
  SYSTEM.GC;
  x.val := 3;
  Out.Int(a[0].val, 0); Out.Ln  
END P;

BEGIN
  NEW(x); NEW(x);
  b[0] := x;
  x.val := 2;
  P(b)
END tParamGC07.

(*<<
3
>>*)

(*[[
!! (SYMFILE #tParamGC07 STAMP #tParamGC07.%main 1 #tParamGC07.m)
!! (CHKSUM STAMP)
!! 
MODULE tParamGC07 STAMP 0
IMPORT Out STAMP
ENDHDR

PROC tParamGC07.P 0 4 0x00100001
! PROCEDURE P(a: arr);
!   SYSTEM.GC;
GLOBAL SYSTEM.GC
CALL 0
!   x.val := 3;
CONST 3
LDGW tParamGC07.x
NCHECK 17
STOREW
!   Out.Int(a[0].val, 0); Out.Ln  
CONST 0
LDLW 12
LOADW
NCHECK 18
LOADW
GLOBAL Out.Int
CALL 2
GLOBAL Out.Ln
CALL 0
RETURN
END

PROC tParamGC07.%main 0 4 0
!   NEW(x); NEW(x);
CONST 8
GLOBAL tParamGC07.blob
GLOBAL NEW
CALLW 2
STGW tParamGC07.x
CONST 8
GLOBAL tParamGC07.blob
GLOBAL NEW
CALLW 2
STGW tParamGC07.x
!   b[0] := x;
LDGW tParamGC07.x
STGW tParamGC07.b
!   x.val := 2;
CONST 2
LDGW tParamGC07.x
NCHECK 24
STOREW
!   P(b)
GLOBAL tParamGC07.b
GLOBAL tParamGC07.P
CALL 1
RETURN
END

! Global variables
GLOVAR tParamGC07.x 4
GLOVAR tParamGC07.b 8

! Global pointer map
DEFINE tParamGC07.%gcmap
WORD GC_POINTER
WORD tParamGC07.x
WORD GC_BASE
WORD tParamGC07.b
WORD 0x00000007
WORD GC_END

! Descriptor for blob
DEFINE tParamGC07.blob
WORD 0x00000005
WORD 0
WORD tParamGC07.blob.%anc

DEFINE tParamGC07.blob.%anc
WORD tParamGC07.blob

! End of file
]]*)
