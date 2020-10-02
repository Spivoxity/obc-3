MODULE tNewBug07;

IMPORT Out;

TYPE ptr = POINTER TO blob;
  blob = RECORD data: INTEGER; next: ptr END;

VAR p, q: ptr;

PROCEDURE GcDebug(flags: ARRAY OF CHAR) IS "gc_debug";

BEGIN
  GcDebug("z");
  NEW(p); NEW(q); p := NIL; NEW(q.next);
  IF q.next # NIL THEN
    Out.String("Pass")
  ELSE
    Out.String("Fail")
  END;
  Out.Ln
END tNewBug07.

(*<<
Pass
>>*)

(*[[
!! (SYMFILE #tNewBug07 STAMP #tNewBug07.%main 1 #tNewBug07.m)
!! (CHKSUM STAMP)
!! 
MODULE tNewBug07 STAMP 0
IMPORT Out STAMP
ENDHDR

PRIMDEF tNewBug07.GcDebug gc_debug VX

PROC tNewBug07.%main 0 3 0
!   GcDebug("z");
CONST 2
GLOBAL tNewBug07.%3
GLOBAL tNewBug07.GcDebug
CALL 2
!   NEW(p); NEW(q); p := NIL; NEW(q.next);
CONST 8
GLOBAL tNewBug07.blob
GLOBAL NEW
CALLW 2
STGW tNewBug07.p
CONST 8
GLOBAL tNewBug07.blob
GLOBAL NEW
CALLW 2
STGW tNewBug07.q
CONST 0
STGW tNewBug07.p
CONST 8
GLOBAL tNewBug07.blob
GLOBAL NEW
CALLW 2
LDGW tNewBug07.q
NCHECK 14
STNW 4
!   IF q.next # NIL THEN
LDGW tNewBug07.q
NCHECK 15
LDNW 4
JEQZ L6
!     Out.String("Pass")
CONST 5
GLOBAL tNewBug07.%1
GLOBAL Out.String
CALL 2
JUMP L4
LABEL L6
!     Out.String("Fail")
CONST 5
GLOBAL tNewBug07.%2
GLOBAL Out.String
CALL 2
LABEL L4
!   Out.Ln
GLOBAL Out.Ln
CALL 0
RETURN
END

! Global variables
GLOVAR tNewBug07.p 4
GLOVAR tNewBug07.q 4

! Global pointer map
DEFINE tNewBug07.%gcmap
WORD GC_POINTER
WORD tNewBug07.p
WORD GC_POINTER
WORD tNewBug07.q
WORD GC_END

! String "Pass"
DEFINE tNewBug07.%1
STRING 5061737300

! String "Fail"
DEFINE tNewBug07.%2
STRING 4661696C00

! String "z"
DEFINE tNewBug07.%3
STRING 7A00

! Descriptor for blob
DEFINE tNewBug07.blob
WORD 0x00000005
WORD 0
WORD tNewBug07.blob.%anc

DEFINE tNewBug07.blob.%anc
WORD tNewBug07.blob

! End of file
]]*)
