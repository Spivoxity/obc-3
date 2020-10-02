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
!! (SYMFILE #tChars STAMP #tChars.%main 1 #tChars.m)
!! (CHKSUM STAMP)
!! 
MODULE tChars STAMP 0
IMPORT Out STAMP
ENDHDR

PROC tChars.%main 0 4 0
!   Out.Char("c");
CONST 99
ALIGNC
GLOBAL Out.Char
CALL 1
!   Out.String("");
CONST 1
GLOBAL tChars.%1
GLOBAL Out.String
CALL 2
!   Out.String("w");
CONST 2
GLOBAL tChars.%3
GLOBAL Out.String
CALL 2
!   Out.String("xx");
CONST 3
GLOBAL tChars.%2
GLOBAL Out.String
CALL 2
!   x[0] := 'y'; x[1] := 0X;
CONST 121
STGC tChars.x
CONST 0
GLOBAL tChars.x
CONST 1
STIC
!   Out.String(x);
CONST 3
GLOBAL tChars.x
GLOBAL Out.String
CALL 2
!   Out.Ln
GLOBAL Out.Ln
CALL 0
RETURN
END

! Global variables
GLOVAR tChars.x 3

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
