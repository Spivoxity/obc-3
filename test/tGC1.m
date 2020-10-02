MODULE tGC1;

(*<<
Done 2097152
>>*)

IMPORT Out;

TYPE ptr = POINTER TO ARRAY 256 OF INTEGER;

VAR i: INTEGER; p: ptr;

PROCEDURE GcHeapSize(): INTEGER IS "gc_heap_size";

BEGIN
  FOR i := 1 TO 128 * 1024 DO
    NEW(p)
  END;
  Out.String("Done "); Out.Int(GcHeapSize(), 0); Out.Ln
END tGC1.

(*[[
!! (SYMFILE #tGC1 STAMP #tGC1.%main 1 #tGC1.m)
!! (CHKSUM STAMP)
!! 
MODULE tGC1 STAMP 0
IMPORT Out STAMP
ENDHDR

PRIMDEF tGC1.GcHeapSize gc_heap_size I

PROC tGC1.%main 0 3 0
!   FOR i := 1 TO 128 * 1024 DO
CONST 1
STGW tGC1.i
LABEL L2
LDGW tGC1.i
CONST 131072
JGT L3
!     NEW(p)
CONST 1024
CONST 0
GLOBAL NEW
CALLW 2
STGW tGC1.p
!   FOR i := 1 TO 128 * 1024 DO
LDGW tGC1.i
INC
STGW tGC1.i
JUMP L2
LABEL L3
!   Out.String("Done "); Out.Int(GcHeapSize(), 0); Out.Ln
CONST 6
GLOBAL tGC1.%1
GLOBAL Out.String
CALL 2
CONST 0
GLOBAL tGC1.GcHeapSize
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

! Global pointer map
DEFINE tGC1.%gcmap
WORD GC_POINTER
WORD tGC1.p
WORD GC_END

! String "Done "
DEFINE tGC1.%1
STRING 446F6E652000

! End of file
]]*)
