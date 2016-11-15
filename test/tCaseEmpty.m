MODULE tCaseEmpty;

(*<<
>>*)

BEGIN
  CASE 0 OF ELSE END
END tCaseEmpty.

(*[[
!! (SYMFILE #tCaseEmpty 0x00000301 #tCaseEmpty.%main 1)
!! (CHKSUM 0x5b2ae2e3)
!! 
MODULE tCaseEmpty STAMP 0
ENDHDR

PROC tCaseEmpty.%main 0 1 0
!   CASE 0 OF ELSE END
RETURN
END

! End of file
]]*)
