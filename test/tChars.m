MODULE tChars;

(*<<
cwxxy
>>*)

IMPORT Out;

VAR x: ARRAY 3 OF CHAR;

BEGIN
  Out.Char("c");
  Out.String("");
  Out.String("w");
  Out.String("xx");
  x[0] := 'y'; x[1] := 0X;
  Out.String(x);
  Out.Ln
END tChars.

(*[[
!! SYMFILE #tChars STAMP #tChars.%main 1
!! END STAMP
!! 
MODULE tChars STAMP 0
IMPORT Out STAMP
ENDHDR

PROC tChars.%main 0 16 0
!   Out.Char("c");
CONST 99
ALIGNC
CONST Out.Char
CALL 1
!   Out.String("");
CONST 1
CONST tChars.%1
CONST Out.String
CALL 2
!   Out.String("w");
CONST 2
CONST tChars.%3
CONST Out.String
CALL 2
!   Out.String("xx");
CONST 3
CONST tChars.%2
CONST Out.String
CALL 2
!   x[0] := 'y'; x[1] := 0X;
CONST 121
STGC tChars.x
CONST 0
CONST tChars.x
CONST 1
STIC
!   Out.String(x);
CONST 3
CONST tChars.x
CONST Out.String
CALL 2
!   Out.Ln
CONST Out.Ln
CALL 0
RETURN
END

! Global variables
GLOBAL tChars.x 3

! String ""
DEFINE tChars.%1
STRING 00

! String "xx"
DEFINE tChars.%2
STRING 787800

! String "w"
DEFINE tChars.%3
STRING 7700

! End of file
]]*)
