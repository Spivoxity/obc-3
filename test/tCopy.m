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
!! SYMFILE #tCopy STAMP #tCopy.%main 1
!! END STAMP
!! 
MODULE tCopy STAMP 0
IMPORT Out STAMP
ENDHDR

PROC tCopy.%main 0 20 0
!   COPY("foo", s);
CONST 10
CONST tCopy.s
CONST 4
CONST tCopy.%1
CONST COPY
CALL 4
!   Out.String(s); Out.Ln
CONST 10
CONST tCopy.s
CONST Out.String
CALL 2
CONST Out.Ln
CALL 0
RETURN
END

! Global variables
GLOBAL tCopy.s 10

! String "foo"
DEFINE tCopy.%1
STRING 666F6F00

! End of file
]]*)
