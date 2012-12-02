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
!! SYMFILE #tTypeRef STAMP #tTypeRef.%main 1
!! END STAMP
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
GLOBAL tTypeRef.x
GLOBAL NEW
CALL 3
LDGW tTypeRef.x
STGW tTypeRef.p
LDGW tTypeRef.p
DUP 0
NCHECK 13
LDNW -4
GLOBAL xPrelude.xrec
TYPETEST 1
JUMPT 1
ERROR E_CAST 13
LABEL 1
STGW tTypeRef.x
RETURN
END

! Global variables
GLOVAR tTypeRef.x 4
GLOVAR tTypeRef.p 4

! Pointer map
DEFINE tTypeRef.%gcmap
WORD GC_BASE
WORD tTypeRef.x
WORD 0
WORD GC_BASE
WORD tTypeRef.p
WORD 0
WORD GC_END

! End of file
]]*)
