MODULE tConst;

IMPORT Out;

PROCEDURE Print(s: ARRAY OF CHAR; n: INTEGER);
BEGIN
  Out.String(s); Out.Char(' '); Out.Int(n, 0); Out.Ln
END Print;

BEGIN
  Print("127", 127);
  Print("128", 128);
  Print("255", 255);
  Print("-127", -127);
  Print("-128", -128);
  Print("-129", -129)
END tConst.

(*<<
127 127
128 128
255 255
-127 -127
-128 -128
-129 -129
>>*)

(*[[
!! SYMFILE #tConst STAMP #tConst.%main 1
!! END STAMP
!! 
MODULE tConst STAMP 0
IMPORT Out STAMP
ENDHDR

PROC tConst.Print 0 12 0
! PROCEDURE Print(s: ARRAY OF CHAR; n: INTEGER);
LOCAL 12
LDLW 16
FLEXCOPY
!   Out.String(s); Out.Char(' '); Out.Int(n, 0); Out.Ln
LDLW 16
LDLW 12
CONST Out.String
CALL 2
CONST 32
ALIGNC
CONST Out.Char
CALL 1
CONST 0
LDLW 20
CONST Out.Int
CALL 2
CONST Out.Ln
CALL 0
RETURN
END

PROC tConst.%main 0 16 0
!   Print("127", 127);
CONST 127
CONST 4
CONST tConst.%1
CONST tConst.Print
CALL 3
!   Print("128", 128);
CONST 128
CONST 4
CONST tConst.%2
CONST tConst.Print
CALL 3
!   Print("255", 255);
CONST 255
CONST 4
CONST tConst.%3
CONST tConst.Print
CALL 3
!   Print("-127", -127);
CONST -127
CONST 5
CONST tConst.%4
CONST tConst.Print
CALL 3
!   Print("-128", -128);
CONST -128
CONST 5
CONST tConst.%5
CONST tConst.Print
CALL 3
!   Print("-129", -129)
CONST -129
CONST 5
CONST tConst.%6
CONST tConst.Print
CALL 3
RETURN
END

! String "127"
DEFINE tConst.%1
STRING 31323700

! String "128"
DEFINE tConst.%2
STRING 31323800

! String "255"
DEFINE tConst.%3
STRING 32353500

! String "-127"
DEFINE tConst.%4
STRING 2D31323700

! String "-128"
DEFINE tConst.%5
STRING 2D31323800

! String "-129"
DEFINE tConst.%6
STRING 2D31323900

! End of file
]]*)
