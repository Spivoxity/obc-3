MODULE tTypeRef;

(* Check references to imported types *)

IMPORT xTypes, xPrelude;

VAR x: xPrelude.xptr; p: xTypes.ptr;

BEGIN
  xTypes.r99.x := 345;
  xPrelude.rPrint(xTypes.r99);

  NEW(x); p := x; x := p(xPrelude.xptr)
END tTypeRef.

(*<<
345
>>*)

(*[[
!! (SYMFILE #tTypeRef STAMP #tTypeRef.%main 1 #tTypeRef.m)
!! (CHKSUM STAMP)
!! 
MODULE tTypeRef STAMP 0
IMPORT xTypes STAMP
IMPORT xPrelude STAMP
ENDHDR

PROC tTypeRef.%main 0 4 0
!   xTypes.r99.x := 345;
CONST 345
STGW xTypes.r99
!   xPrelude.rPrint(xTypes.r99);
GLOBAL xTypes.rec
GLOBAL xTypes.r99
GLOBAL xPrelude.rPrint
CALL 2
!   NEW(x); p := x; x := p(xPrelude.xptr)
CONST 4
GLOBAL xPrelude.xrec
GLOBAL NEW
CALLW 2
STGW tTypeRef.x
LDGW tTypeRef.x
STGW tTypeRef.p
LDGW tTypeRef.p
DUP 0
NCHECK 13
LDNW -4
DUP 0
LDNW 4
CONST 1
JGEQ L3
POP 1
JUMP L2
LABEL L3
LDNW 8
LDNW 4
GLOBAL xPrelude.xrec
JEQ L1
LABEL L2
ERROR E_CAST 13
LABEL L1
STGW tTypeRef.x
RETURN
END

! Global variables
GLOVAR tTypeRef.x 4
GLOVAR tTypeRef.p 4

! Global pointer map
DEFINE tTypeRef.%gcmap
WORD GC_POINTER
WORD tTypeRef.x
WORD GC_POINTER
WORD tTypeRef.p
WORD GC_END

! End of file
]]*)
