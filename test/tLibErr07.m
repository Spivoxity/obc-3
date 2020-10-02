MODULE tLibErr07;

(*<<
Runtime error: string is not null-terminated
In procedure COMPARE
   called from tLibErr07.%main
   called from MAIN
>>*)

VAR t: ARRAY 2 OF CHAR; b: BOOLEAN;

BEGIN
  t[0] := 'a'; t[1] := 'b';
  b := (t = 'ab')
END tLibErr07.
  
(*[[
!! (SYMFILE #tLibErr07 STAMP #tLibErr07.%main 1 #tLibErr07.m)
!! (CHKSUM STAMP)
!! 
MODULE tLibErr07 STAMP 0
ENDHDR

PROC tLibErr07.%main 0 5 0
!   t[0] := 'a'; t[1] := 'b';
CONST 97
STGC tLibErr07.t
CONST 98
GLOBAL tLibErr07.t
CONST 1
STIC
!   b := (t = 'ab')
CONST 3
GLOBAL tLibErr07.%1
CONST 2
GLOBAL tLibErr07.t
GLOBAL COMPARE
CALLW 4
CONST 0
EQ
STGC tLibErr07.b
RETURN
END

! Global variables
GLOVAR tLibErr07.t 2
GLOVAR tLibErr07.b 1

! String "ab"
DEFINE tLibErr07.%1
STRING 616200

! End of file
]]*)
