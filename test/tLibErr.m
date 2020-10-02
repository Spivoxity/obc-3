MODULE tLibErr;

(*<<
Runtime error: string is not null-terminated
In procedure COMPARE
   called from tLibErr.%main
   called from MAIN
>>*)

VAR t: ARRAY 2 OF CHAR; b: BOOLEAN;

BEGIN
  t[0] := 'a'; t[1] := 'b';
  b := (t = 'ab')
END tLibErr.
  
(*[[
!! (SYMFILE #tLibErr STAMP #tLibErr.%main 1 #tLibErr.m)
!! (CHKSUM STAMP)
!! 
MODULE tLibErr STAMP 0
ENDHDR

PROC tLibErr.%main 0 5 0
!   t[0] := 'a'; t[1] := 'b';
CONST 97
STGC tLibErr.t
CONST 98
GLOBAL tLibErr.t
CONST 1
STIC
!   b := (t = 'ab')
CONST 3
GLOBAL tLibErr.%1
CONST 2
GLOBAL tLibErr.t
GLOBAL COMPARE
CALLW 4
CONST 0
EQ
STGC tLibErr.b
RETURN
END

! Global variables
GLOVAR tLibErr.t 2
GLOVAR tLibErr.b 1

! String "ab"
DEFINE tLibErr.%1
STRING 616200

! End of file
]]*)
