MODULE tBigInt07;

(*<<
2147483646
>>*)

IMPORT Out, Random;

BEGIN
  Out.Int(Random.MAXRAND, 0); Out.Ln
END tBigInt07.

(*[[
!! (SYMFILE #tBigInt07 0x00000301 #tBigInt07.%main 1)
!! (CHKSUM 0x79569f35)
!! 
MODULE tBigInt07 STAMP 0
IMPORT Out STAMP
IMPORT Random STAMP
ENDHDR

PROC tBigInt07.%main 0 3 0
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
