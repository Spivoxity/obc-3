MODULE tAggreg07;

IMPORT Out;

PROCEDURE Print(s: ARRAY OF CHAR);
BEGIN
  Out.Int(LEN(s), 0);
  Out.String(s);
  Out.Ln
END Print;

BEGIN
  Print("Hello!")
END tAggreg07.

(*<<
7Hello!
>>*)

(*[[
!! (SYMFILE #tAggreg07 STAMP #tAggreg07.%main 1 #tAggreg07.m)
!! (CHKSUM STAMP)
!! 
MODULE tAggreg07 STAMP 0
IMPORT Out STAMP
ENDHDR

PROC tAggreg07.Print 0 3 0x00100001
! PROCEDURE Print(s: ARRAY OF CHAR);
!   Out.Int(LEN(s), 0);
CONST 0
LDLW 16
GLOBAL Out.Int
CALL 2
!   Out.String(s);
LDLW 16
LDLW 12
GLOBAL Out.String
CALL 2
!   Out.Ln
GLOBAL Out.Ln
CALL 0
RETURN
END

PROC tAggreg07.%main 0 3 0
!   Print("Hello!")
CONST 7
GLOBAL tAggreg07.%1
GLOBAL tAggreg07.Print
CALL 2
RETURN
END

! String "Hello!"
DEFINE tAggreg07.%1
STRING 48656C6C6F2100

! End of file
]]*)
