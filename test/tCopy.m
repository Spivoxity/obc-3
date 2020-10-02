MODULE tCopy;

(* Bug: built-in procedures had wrong number 
   of arguments in CALL instruction *)

(*<<
foo
>>*)

IMPORT Out;

VAR s: ARRAY 10 OF CHAR;

BEGIN
  COPY("foo", s);
  Out.String(s); Out.Ln
END tCopy.

(*[[
!! (SYMFILE #tCopy STAMP #tCopy.%main 1 #tCopy.m)
!! (CHKSUM STAMP)
!! 
MODULE tCopy STAMP 0
IMPORT Out STAMP
ENDHDR

PROC tCopy.%main 0 5 0
!   COPY("foo", s);
CONST 10
GLOBAL tCopy.s
CONST 4
GLOBAL tCopy.%1
GLOBAL COPY
CALL 4
!   Out.String(s); Out.Ln
CONST 10
GLOBAL tCopy.s
GLOBAL Out.String
CALL 2
GLOBAL Out.Ln
CALL 0
RETURN
END

! Global variables
GLOVAR tCopy.s 10

! String "foo"
DEFINE tCopy.%1
STRING 666F6F00

! End of file
]]*)
