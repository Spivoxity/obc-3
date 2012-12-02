MODULE tBigInt;

(*<<
2147483646
>>*)

IMPORT Out, Random;

BEGIN
  Out.Int(Random.MAXRAND, 0); Out.Ln
END tBigInt.

(*[[
!! SYMFILE #tBigInt STAMP #tBigInt.%main 1
!! END STAMP
!! 
MODULE tBigInt STAMP 0
IMPORT Out STAMP
IMPORT Random STAMP
ENDHDR

PROC tBigInt.%main 0 12 0
!   Out.Int(Random.MAXRAND, 0); Out.Ln
CONST 0
CONST 2147483646
CONST Out.Int
CALL 2
CONST Out.Ln
CALL 0
RETURN
END

! End of file
]]*)
