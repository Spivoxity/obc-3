MODULE tNotOpen;

IMPORT Files;

VAR f: Files.File;

BEGIN
  Files.Close(f)
END tNotOpen.

(*<<
Runtime error: file is not open in module Files
In procedure Files.raw
   called from Files.Close
   called from tNotOpen.%main
   called from MAIN
>>*)

(*[[
!! (SYMFILE #tNotOpen STAMP #tNotOpen.%main 1)
!! (CHKSUM STAMP)
!! 
MODULE tNotOpen STAMP 0
IMPORT Files STAMP
ENDHDR

PROC tNotOpen.%main 0 2 0
!   Files.Close(f)
LDGW tNotOpen.f
GLOBAL Files.Close
CALL 1
RETURN
END

! Global variables
GLOVAR tNotOpen.f 4

! Pointer map
DEFINE tNotOpen.%gcmap
WORD GC_BASE
WORD tNotOpen.f
WORD 0
WORD GC_END

! End of file
]]*)
