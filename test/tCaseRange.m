MODULE tCaseRange;

IMPORT Out;

PROCEDURE Classify(n: INTEGER);
BEGIN
  Out.Int(n, 0); Out.String(" is in the ");
  CASE n OF
      10..99: Out.String("Tens")
    | 100..999: Out.String("Hundreds")
    | 1000..9999: Out.String("Thousands")
    | 10000..99999: Out.String("Tens of thousands")
    | 100000..999999: Out.String("Hundreds of thousands")
    | 1000000..9999999: Out.String("Millions")
  END;
  Out.Ln
END Classify;

VAR x: INTEGER;
BEGIN
  x := 20;
  REPEAT
    Classify(x);
    x := 10*x
  UNTIL x > 10000000
END tCaseRange.

(*<<
20 is in the Tens
200 is in the Hundreds
2000 is in the Thousands
20000 is in the Tens of thousands
200000 is in the Hundreds of thousands
2000000 is in the Millions
>>*)

(*[[
!! (SYMFILE #tCaseRange STAMP #tCaseRange.%main 1 #tCaseRange.m)
!! (CHKSUM STAMP)
!! 
MODULE tCaseRange STAMP 0
IMPORT Out STAMP
ENDHDR

PROC tCaseRange.Classify 0 3 0
! PROCEDURE Classify(n: INTEGER);
!   Out.Int(n, 0); Out.String(" is in the ");
CONST 0
LDLW 12
GLOBAL Out.Int
CALL 2
CONST 12
GLOBAL tCaseRange.%1
GLOBAL Out.String
CALL 2
!   CASE n OF
LDLW 12
CONST 10000
TESTGEQ L16
CONST 1000
TESTGEQ L19
CONST 100
TESTGEQ L20
CONST 10
JGEQ L10
JUMP L8
LABEL L20
POP 1
JUMP L11
LABEL L19
POP 1
JUMP L12
LABEL L16
CONST 1000000
TESTGEQ L17
CONST 100000
TESTGEQ L18
POP 1
JUMP L13
LABEL L18
POP 1
JUMP L14
LABEL L17
CONST 9999999
JLEQ L15
JUMP L8
LABEL L10
!       10..99: Out.String("Tens")
CONST 5
GLOBAL tCaseRange.%2
GLOBAL Out.String
CALL 2
JUMP L9
LABEL L11
!     | 100..999: Out.String("Hundreds")
CONST 9
GLOBAL tCaseRange.%3
GLOBAL Out.String
CALL 2
JUMP L9
LABEL L12
!     | 1000..9999: Out.String("Thousands")
CONST 10
GLOBAL tCaseRange.%4
GLOBAL Out.String
CALL 2
JUMP L9
LABEL L13
!     | 10000..99999: Out.String("Tens of thousands")
CONST 18
GLOBAL tCaseRange.%5
GLOBAL Out.String
CALL 2
JUMP L9
LABEL L14
!     | 100000..999999: Out.String("Hundreds of thousands")
CONST 22
GLOBAL tCaseRange.%6
GLOBAL Out.String
CALL 2
JUMP L9
LABEL L15
!     | 1000000..9999999: Out.String("Millions")
CONST 9
GLOBAL tCaseRange.%7
GLOBAL Out.String
CALL 2
JUMP L9
LABEL L8
ERROR E_CASE 8
LABEL L9
!   Out.Ln
GLOBAL Out.Ln
CALL 0
RETURN
END

PROC tCaseRange.%main 0 2 0
!   x := 20;
CONST 20
STGW tCaseRange.x
LABEL L21
!     Classify(x);
LDGW tCaseRange.x
GLOBAL tCaseRange.Classify
CALL 1
!     x := 10*x
LDGW tCaseRange.x
CONST 10
TIMES
STGW tCaseRange.x
!   UNTIL x > 10000000
LDGW tCaseRange.x
CONST 10000000
JLEQ L21
RETURN
END

! Global variables
GLOVAR tCaseRange.x 4

! String " is in the "
DEFINE tCaseRange.%1
STRING 20697320696E207468652000

! String "Tens"
DEFINE tCaseRange.%2
STRING 54656E7300

! String "Hundreds"
DEFINE tCaseRange.%3
STRING 48756E647265647300

! String "Thousands"
DEFINE tCaseRange.%4
STRING 54686F7573616E647300

! String "Tens of thousands"
DEFINE tCaseRange.%5
STRING 54656E73206F662074686F7573616E647300

! String "Hundreds of thousands"
DEFINE tCaseRange.%6
STRING 48756E6472656473206F662074686F7573616E647300

! String "Millions"
DEFINE tCaseRange.%7
STRING 4D696C6C696F6E7300

! End of file
]]*)
