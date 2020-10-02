MODULE tConst07;

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
END tConst07.

(*<<
127 127
128 128
255 255
-127 -127
-128 -128
-129 -129
>>*)

(*[[
!! (SYMFILE #tConst07 STAMP #tConst07.%main 1 #tConst07.m)
!! (CHKSUM STAMP)
!! 
MODULE tConst07 STAMP 0
IMPORT Out STAMP
ENDHDR

PROC tConst07.Print 0 3 0x00100001
! PROCEDURE Print(s: ARRAY OF CHAR; n: INTEGER);
!   Out.String(s); Out.Char(' '); Out.Int(n, 0); Out.Ln
LDLW 16
LDLW 12
GLOBAL Out.String
CALL 2
CONST 32
ALIGNC
GLOBAL Out.Char
CALL 1
CONST 0
LDLW 20
GLOBAL Out.Int
CALL 2
GLOBAL Out.Ln
CALL 0
RETURN
END

PROC tConst07.%main 0 4 0
!   Print("127", 127);
CONST 127
CONST 4
GLOBAL tConst07.%1
GLOBAL tConst07.Print
CALL 3
!   Print("128", 128);
CONST 128
CONST 4
GLOBAL tConst07.%2
GLOBAL tConst07.Print
CALL 3
!   Print("255", 255);
CONST 255
CONST 4
GLOBAL tConst07.%3
GLOBAL tConst07.Print
CALL 3
!   Print("-127", -127);
CONST -127
CONST 5
GLOBAL tConst07.%4
GLOBAL tConst07.Print
CALL 3
!   Print("-128", -128);
CONST -128
CONST 5
GLOBAL tConst07.%5
GLOBAL tConst07.Print
CALL 3
!   Print("-129", -129)
CONST -129
CONST 5
GLOBAL tConst07.%6
GLOBAL tConst07.Print
CALL 3
RETURN
END

! String "127"
DEFINE tConst07.%1
STRING 31323700

! String "128"
DEFINE tConst07.%2
STRING 31323800

! String "255"
DEFINE tConst07.%3
STRING 32353500

! String "-127"
DEFINE tConst07.%4
STRING 2D31323700

! String "-128"
DEFINE tConst07.%5
STRING 2D31323800

! String "-129"
DEFINE tConst07.%6
STRING 2D31323900

! End of file
]]*)
