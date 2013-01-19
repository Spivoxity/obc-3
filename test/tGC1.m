MODULE tGC1;

(*<<
Done 2097152
>>*)

IMPORT Out, GC;

TYPE ptr = POINTER TO ARRAY 256 OF INTEGER;

VAR i: INTEGER; p: ptr;

BEGIN
  FOR i := 1 TO 128 * 1024 DO
    NEW(p)
  END;
  Out.String("Done "); Out.Int(GC.HeapSize(), 0); Out.Ln
END tGC1.

(*[[
!! SYMFILE #tGC1 STAMP #tGC1.%main 1
!! END STAMP
!! 
MODULE tGC1 STAMP 0
IMPORT Out STAMP
IMPORT GC STAMP
ENDHDR

PROC tGC1.%main 0 4 0
!   FOR i := 1 TO 128 * 1024 DO
CONST 1
STGW tGC1.i
LABEL 2
LDGW tGC1.i
CONST 131072
JGT 3
!     NEW(p)
CONST 1024
CONST 0
GLOBAL tGC1.p
GLOBAL NEW
CALL 3
LDGW tGC1.i
INC
STGW tGC1.i
JUMP 2
LABEL 3
!   Out.String("Done "); Out.Int(GC.HeapSize(), 0); Out.Ln
CONST 6
GLOBAL tGC1.%1
GLOBAL Out.String
CALL 2
CONST 0
GLOBAL GC.HeapSize
CALLW 0
GLOBAL Out.Int
CALL 2
GLOBAL Out.Ln
CALL 0
RETURN
END

! Global variables
GLOVAR tGC1.i 4
GLOVAR tGC1.p 4

! Pointer map
DEFINE tGC1.%gcmap
WORD GC_BASE
WORD tGC1.p
WORD 0
WORD GC_END

! String "Done "
DEFINE tGC1.%1
STRING 446F6E652000

! End of file
]]*)
