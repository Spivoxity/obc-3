MODULE tBigInt;

(*<<
2147483646
>>*)

IMPORT Out, Random;

BEGIN
  Out.Int(Random.MAXRAND, 0); Out.Ln
END tBigInt.

(*[[
!! (SYMFILE #tBigInt STAMP #tBigInt.%main 1 #tBigInt.m)
!! (CHKSUM STAMP)
!! 
MODULE tBigInt STAMP 0
IMPORT Out STAMP
IMPORT Random STAMP
ENDHDR

PROC tBigInt.%main 0 3 0
!   Out.Int(Random.MAXRAND, 0); Out.Ln
CONST 0
CONST 2147483646
GLOBAL Out.Int
CALL 2
GLOBAL Out.Ln
CALL 0
RETURN
END

! End of file
]]*)
