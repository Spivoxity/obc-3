MODULE tFlexAss07;

IMPORT Out;

PROCEDURE Print(s: ARRAY OF CHAR);
  VAR a: ARRAY 1024 OF CHAR;
BEGIN
  a := s;
  Out.String(a); Out.Ln
END Print;

BEGIN
  Print("Hello world")
END tFlexAss07.

(*<<
Hello world
>>*)

(*[[
!! (SYMFILE #tFlexAss07 STAMP #tFlexAss07.%main 1 #tFlexAss07.m)
!! (CHKSUM STAMP)
!! 
MODULE tFlexAss07 STAMP 0
IMPORT Out STAMP
ENDHDR

PROC tFlexAss07.Print 1024 7 0x00100001
! PROCEDURE Print(s: ARRAY OF CHAR);
!   a := s;
CONST 1024
LOCAL -1024
LDLW 16
LDLW 12
CONST 1
CONST 1
GLOBAL FLEXASSIGN
CALL 6
!   Out.String(a); Out.Ln
CONST 1024
LOCAL -1024
GLOBAL Out.String
CALL 2
GLOBAL Out.Ln
CALL 0
RETURN
END

PROC tFlexAss07.%main 0 3 0
!   Print("Hello world")
CONST 12
GLOBAL tFlexAss07.%1
GLOBAL tFlexAss07.Print
CALL 2
RETURN
END

! String "Hello world"
DEFINE tFlexAss07.%1
STRING 48656C6C6F20776F726C6400

! End of file
]]*)
