MODULE tCopy07;

(* Bug: built-in procedures had wrong number 
   of arguments in CALL instruction *)

(*<<
foo
>>*)

IMPORT Out;

VAR s: ARRAY 10 OF CHAR;

BEGIN
  s := "foo";
  Out.String(s); Out.Ln
END tCopy07.

(*[[
!! (SYMFILE #tCopy07 STAMP #tCopy07.%main 1 #tCopy07.m)
!! (CHKSUM STAMP)
!! 
MODULE tCopy07 STAMP 0
IMPORT Out STAMP
ENDHDR

PROC tCopy07.%main 0 5 0
!   s := "foo";
CONST 10
GLOBAL tCopy07.s
CONST 4
GLOBAL tCopy07.%1
GLOBAL COPY
CALL 4
!   Out.String(s); Out.Ln
CONST 10
GLOBAL tCopy07.s
GLOBAL Out.String
CALL 2
GLOBAL Out.Ln
CALL 0
RETURN
END

! Global variables
GLOVAR tCopy07.s 10

! String "foo"
DEFINE tCopy07.%1
STRING 666F6F00

! End of file
]]*)
