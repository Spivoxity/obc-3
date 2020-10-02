MODULE tChars07;

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
END tChars07.

(*[[
!! (SYMFILE #tChars07 STAMP #tChars07.%main 1 #tChars07.m)
!! (CHKSUM STAMP)
!! 
MODULE tChars07 STAMP 0
IMPORT Out STAMP
ENDHDR

PROC tChars07.%main 0 4 0
!   Out.Char("c");
CONST 99
ALIGNC
GLOBAL Out.Char
CALL 1
!   Out.String("");
CONST 1
GLOBAL tChars07.%1
GLOBAL Out.String
CALL 2
!   Out.String("w");
CONST 2
GLOBAL tChars07.%3
GLOBAL Out.String
CALL 2
!   Out.String("xx");
CONST 3
GLOBAL tChars07.%2
GLOBAL Out.String
CALL 2
!   x[0] := 'y'; x[1] := 0X;
CONST 121
STGC tChars07.x
CONST 0
GLOBAL tChars07.x
CONST 1
STIC
!   Out.String(x);
CONST 3
GLOBAL tChars07.x
GLOBAL Out.String
CALL 2
!   Out.Ln
GLOBAL Out.Ln
CALL 0
RETURN
END

! Global variables
GLOVAR tChars07.x 3

! String ""
DEFINE tChars07.%1
STRING 00

! String "xx"
DEFINE tChars07.%2
STRING 787800

! String "w"
DEFINE tChars07.%3
STRING 7700

! End of file
]]*)
