MODULE tSwap07;
IMPORT Out;
VAR y: INTEGER;
BEGIN
  VAR x, z: INTEGER;
  BEGIN
    x, y := 3, 4;
    x, y := y, x;
    Out.Int(x, 0); Out.Int(y, 2); Out.Ln;
    x, y, z := 7, 8, 9;
    x, y, z := y, z, x;
    Out.Int(x, 0); Out.Int(y, 2); Out.Int(z, 2); Out.Ln
  END
END tSwap07.

(*<<
4 3
8 9 7
>>*)

(*[[
!! SYMFILE #tSwap07 STAMP #tSwap07.%main 1
!! END STAMP
!! 
MODULE tSwap07 STAMP 0
IMPORT Out STAMP
ENDHDR

PROC tSwap07.%main 8 6 0
!     x, y := 3, 4;
CONST 3
CONST 4
STGW tSwap07.y
STLW -4
!     x, y := y, x;
LDGW tSwap07.y
LDLW -4
STGW tSwap07.y
STLW -4
!     Out.Int(x, 0); Out.Int(y, 2); Out.Ln;
CONST 0
LDLW -4
GLOBAL Out.Int
CALL 2
CONST 2
LDGW tSwap07.y
GLOBAL Out.Int
CALL 2
GLOBAL Out.Ln
CALL 0
!     x, y, z := 7, 8, 9;
CONST 7
LOCAL -4
CONST 8
CONST 9
STLW -8
STGW tSwap07.y
STOREW
!     x, y, z := y, z, x;
LDGW tSwap07.y
LOCAL -4
LDLW -8
LDLW -4
STLW -8
STGW tSwap07.y
STOREW
!     Out.Int(x, 0); Out.Int(y, 2); Out.Int(z, 2); Out.Ln
CONST 0
LDLW -4
GLOBAL Out.Int
CALL 2
CONST 2
LDGW tSwap07.y
GLOBAL Out.Int
CALL 2
CONST 2
LDLW -8
GLOBAL Out.Int
CALL 2
GLOBAL Out.Ln
CALL 0
RETURN
END

! Global variables
GLOVAR tSwap07.y 4

! End of file
]]*)
