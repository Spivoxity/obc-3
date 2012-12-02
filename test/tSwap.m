MODULE tSwap;
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
END tSwap.

(*<<
4 3
8 9 7
>>*)

(*[[
!! SYMFILE #tSwap STAMP #tSwap.%main 1
!! END STAMP
!! 
MODULE tSwap STAMP 0
IMPORT Out STAMP
ENDHDR

PROC tSwap.%main 2 24 0
!     x, y := 3, 4;
CONST 3
LOCAL -4
CONST 4
STGW tSwap.y
STOREW
!     x, y := y, x;
LDGW tSwap.y
LOCAL -4
LDLW -4
STGW tSwap.y
STOREW
!     Out.Int(x, 0); Out.Int(y, 2); Out.Ln;
CONST 0
LDLW -4
CONST Out.Int
CALL 2
CONST 2
LDGW tSwap.y
CONST Out.Int
CALL 2
CONST Out.Ln
CALL 0
!     x, y, z := 7, 8, 9;
CONST 7
LOCAL -4
CONST 8
CONST tSwap.y
CONST 9
STLW -8
STOREW
STOREW
!     x, y, z := y, z, x;
LDGW tSwap.y
LOCAL -4
LDLW -8
CONST tSwap.y
LDLW -4
STLW -8
STOREW
STOREW
!     Out.Int(x, 0); Out.Int(y, 2); Out.Int(z, 2); Out.Ln
CONST 0
LDLW -4
CONST Out.Int
CALL 2
CONST 2
LDGW tSwap.y
CONST Out.Int
CALL 2
CONST 2
LDLW -8
CONST Out.Int
CALL 2
CONST Out.Ln
CALL 0
RETURN
END

! Global variables
GLOBAL tSwap.y 4

! End of file
]]*)
