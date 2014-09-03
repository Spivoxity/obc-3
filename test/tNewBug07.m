MODULE tNewBug07;

IMPORT GC, Out;

TYPE ptr = POINTER TO blob;
  blob = RECORD data: INTEGER; next: ptr END;

VAR p, q: ptr;

BEGIN
  GC.Debug("z");
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
!! SYMFILE #tNewBug07 STAMP #tNewBug07.%main 1
!! END STAMP
!! 
MODULE tNewBug07 STAMP 0
IMPORT GC STAMP
IMPORT Out STAMP
ENDHDR

PROC tNewBug07.%main 0 4 0
!   GC.Debug("z");
CONST 2
GLOBAL tNewBug07.%3
GLOBAL GC.Debug
CALL 2
!   NEW(p); NEW(q); p := NIL; NEW(q.next);
CONST 8
GLOBAL tNewBug07.blob
GLOBAL tNewBug07.p
GLOBAL NEW
CALL 3
CONST 8
GLOBAL tNewBug07.blob
GLOBAL tNewBug07.q
GLOBAL NEW
CALL 3
CONST 0
STGW tNewBug07.p
CONST 8
GLOBAL tNewBug07.blob
LDGW tNewBug07.q
NCHECK 12
CONST 4
PLUSA
GLOBAL NEW
CALL 3
!   IF q.next # NIL THEN
LDGW tNewBug07.q
NCHECK 13
LDNW 4
JEQZ 6
!     Out.String("Pass")
CONST 5
GLOBAL tNewBug07.%1
GLOBAL Out.String
CALL 2
JUMP 4
LABEL 6
!     Out.String("Fail")
CONST 5
GLOBAL tNewBug07.%2
GLOBAL Out.String
CALL 2
LABEL 4
!   Out.Ln
GLOBAL Out.Ln
CALL 0
RETURN
END

! Global variables
GLOVAR tNewBug07.p 4
GLOVAR tNewBug07.q 4

! Pointer map
DEFINE tNewBug07.%gcmap
WORD GC_BASE
WORD tNewBug07.p
WORD 0
WORD GC_BASE
WORD tNewBug07.q
WORD 0
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
