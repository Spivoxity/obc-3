MODULE tByte07;

(* Check that BYTE is unsigned *)

IMPORT Out;

VAR a: BYTE;

BEGIN
  a := 200;
  Out.Int(a, 0); Out.Ln
END tByte07.

(*<<
200
>>*)

(*[[
!! (SYMFILE #tByte07 STAMP #tByte07.%main 1 #tByte07.m)
!! (CHKSUM STAMP)
!! 
MODULE tByte07 STAMP 0
IMPORT Out STAMP
ENDHDR

PROC tByte07.%main 0 3 0
!   a := 200;
CONST 200
STGC tByte07.a
!   Out.Int(a, 0); Out.Ln
CONST 0
LDGC tByte07.a
GLOBAL Out.Int
CALL 2
GLOBAL Out.Ln
CALL 0
RETURN
END

! Global variables
GLOVAR tByte07.a 1

! End of file
]]*)
