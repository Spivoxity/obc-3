MODULE tLongCmp07;

IMPORT Out;

(* Unsigned comparison must be used for the low-order word of LONGINT *)

(*<<
Good!
>>*)

VAR m: LONGINT;

BEGIN
  m := 7FFFFFFFH+1;
  IF m >= 0 THEN Out.String("Good!") ELSE Out.String("Bad!") END; 
  Out.Ln
END tLongCmp07.

(*[[
!! (SYMFILE #tLongCmp07 STAMP #tLongCmp07.%main 1 #tLongCmp07.m)
!! (CHKSUM STAMP)
!! 
MODULE tLongCmp07 STAMP 0
IMPORT Out STAMP
ENDHDR

PROC tLongCmp07.%main 0 4 0
!   m := 7FFFFFFFH+1;
QCONST 2147483648
STGQ tLongCmp07.m
!   IF m >= 0 THEN Out.String("Good!") ELSE Out.String("Bad!") END; 
LDGQ tLongCmp07.m
CONST 0
CONVNQ
QJLT L5
CONST 6
GLOBAL tLongCmp07.%1
GLOBAL Out.String
CALL 2
JUMP L3
LABEL L5
CONST 5
GLOBAL tLongCmp07.%2
GLOBAL Out.String
CALL 2
LABEL L3
!   Out.Ln
GLOBAL Out.Ln
CALL 0
RETURN
END

! Global variables
GLOVAR tLongCmp07.m 8

! String "Good!"
DEFINE tLongCmp07.%1
STRING 476F6F642100

! String "Bad!"
DEFINE tLongCmp07.%2
STRING 4261642100

! End of file
]]*)
