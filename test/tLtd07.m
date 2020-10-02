MODULE tLtd07;

IMPORT Out;

VAR a, b: LONGREAL; c: BOOLEAN;

BEGIN
  a := 1.0; b := 2.0;
  c := (a < b);
  IF c THEN Out.String("yes") ELSE Out.String("no") END;
  Out.Ln
END tLtd07.

(*<<
yes
>>*)

(*[[
!! (SYMFILE #tLtd07 STAMP #tLtd07.%main 1 #tLtd07.m)
!! (CHKSUM STAMP)
!! 
MODULE tLtd07 STAMP 0
IMPORT Out STAMP
ENDHDR

PROC tLtd07.%main 0 4 0
!   a := 1.0; b := 2.0;
DCONST 1.0
STGD tLtd07.a
DCONST 2.0
STGD tLtd07.b
!   c := (a < b);
LDGD tLtd07.a
LDGD tLtd07.b
DLT
STGC tLtd07.c
!   IF c THEN Out.String("yes") ELSE Out.String("no") END;
LDGC tLtd07.c
JEQZ L5
CONST 4
GLOBAL tLtd07.%1
GLOBAL Out.String
CALL 2
JUMP L3
LABEL L5
CONST 3
GLOBAL tLtd07.%2
GLOBAL Out.String
CALL 2
LABEL L3
!   Out.Ln
GLOBAL Out.Ln
CALL 0
RETURN
END

! Global variables
GLOVAR tLtd07.a 8
GLOVAR tLtd07.b 8
GLOVAR tLtd07.c 1

! String "yes"
DEFINE tLtd07.%1
STRING 79657300

! String "no"
DEFINE tLtd07.%2
STRING 6E6F00

! End of file
]]*)
