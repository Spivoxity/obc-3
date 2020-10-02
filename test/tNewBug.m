MODULE tNewBug;

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
END tNewBug.

(*<<
Pass
>>*)

(*[[
!! (SYMFILE #tNewBug STAMP #tNewBug.%main 1 #tNewBug.m)
!! (CHKSUM STAMP)
!! 
MODULE tNewBug STAMP 0
IMPORT Out STAMP
ENDHDR

PRIMDEF tNewBug.GcDebug gc_debug VX

PROC tNewBug.%main 0 3 0
!   GcDebug("z");
CONST 2
GLOBAL tNewBug.%3
GLOBAL tNewBug.GcDebug
CALL 2
!   NEW(p); NEW(q); p := NIL; NEW(q.next);
CONST 8
GLOBAL tNewBug.blob
GLOBAL NEW
CALLW 2
STGW tNewBug.p
CONST 8
GLOBAL tNewBug.blob
GLOBAL NEW
CALLW 2
STGW tNewBug.q
CONST 0
STGW tNewBug.p
CONST 8
GLOBAL tNewBug.blob
GLOBAL NEW
CALLW 2
LDGW tNewBug.q
NCHECK 14
STNW 4
!   IF q.next # NIL THEN
LDGW tNewBug.q
NCHECK 15
LDNW 4
JEQZ L6
!     Out.String("Pass")
CONST 5
GLOBAL tNewBug.%1
GLOBAL Out.String
CALL 2
JUMP L4
LABEL L6
!     Out.String("Fail")
CONST 5
GLOBAL tNewBug.%2
GLOBAL Out.String
CALL 2
LABEL L4
!   Out.Ln
GLOBAL Out.Ln
CALL 0
RETURN
END

! Global variables
GLOVAR tNewBug.p 4
GLOVAR tNewBug.q 4

! Global pointer map
DEFINE tNewBug.%gcmap
WORD GC_POINTER
WORD tNewBug.p
WORD GC_POINTER
WORD tNewBug.q
WORD GC_END

! String "Pass"
DEFINE tNewBug.%1
STRING 5061737300

! String "Fail"
DEFINE tNewBug.%2
STRING 4661696C00

! String "z"
DEFINE tNewBug.%3
STRING 7A00

! Descriptor for blob
DEFINE tNewBug.blob
WORD 0x00000005
WORD 0
WORD tNewBug.blob.%anc

DEFINE tNewBug.blob.%anc
WORD tNewBug.blob

! End of file
]]*)
