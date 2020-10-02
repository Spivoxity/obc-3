MODULE tLtd;

IMPORT Out;

VAR a, b: LONGREAL; c: BOOLEAN;

BEGIN
  a := 1.0; b := 2.0;
  c := (a < b);
  IF c THEN Out.String("yes") ELSE Out.String("no") END;
  Out.Ln
END tLtd.

(*<<
yes
>>*)

(*[[
!! (SYMFILE #tLtd STAMP #tLtd.%main 1 #tLtd.m)
!! (CHKSUM STAMP)
!! 
MODULE tLtd STAMP 0
IMPORT Out STAMP
ENDHDR

PROC tLtd.%main 0 4 0
!   a := 1.0; b := 2.0;
DCONST 1.0
STGD tLtd.a
DCONST 2.0
STGD tLtd.b
!   c := (a < b);
LDGD tLtd.a
LDGD tLtd.b
DLT
STGC tLtd.c
!   IF c THEN Out.String("yes") ELSE Out.String("no") END;
LDGC tLtd.c
JEQZ L5
CONST 4
GLOBAL tLtd.%1
GLOBAL Out.String
CALL 2
JUMP L3
LABEL L5
CONST 3
GLOBAL tLtd.%2
GLOBAL Out.String
CALL 2
LABEL L3
!   Out.Ln
GLOBAL Out.Ln
CALL 0
RETURN
END

! Global variables
GLOVAR tLtd.a 8
GLOVAR tLtd.b 8
GLOVAR tLtd.c 1

! String "yes"
DEFINE tLtd.%1
STRING 79657300

! String "no"
DEFINE tLtd.%2
STRING 6E6F00

! End of file
]]*)
