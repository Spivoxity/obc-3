MODULE tByteWidth07; 

IMPORT Out;

TYPE rec = RECORD x, y: BYTE END;

PROCEDURE Set(VAR r: rec); BEGIN r.x := 4; r.y := 5 END Set;

VAR rr: rec;

BEGIN
  Set(rr);
  Out.Int(rr.y, 0);
  Out.Ln
END tByteWidth07.

(*<<
5
>>*)

(*[[
!! (SYMFILE #tByteWidth07 STAMP #tByteWidth07.%main 1 #tByteWidth07.m)
!! (CHKSUM STAMP)
!! 
MODULE tByteWidth07 STAMP 0
IMPORT Out STAMP
ENDHDR

PROC tByteWidth07.Set 0 3 0x00100001
! PROCEDURE Set(VAR r: rec); BEGIN r.x := 4; r.y := 5 END Set;
CONST 4
LDLW 12
STOREC
CONST 5
LDLW 12
CONST 1
STIC
RETURN
END

PROC tByteWidth07.%main 0 3 0
!   Set(rr);
GLOBAL tByteWidth07.rec
GLOBAL tByteWidth07.rr
GLOBAL tByteWidth07.Set
CALL 2
!   Out.Int(rr.y, 0);
CONST 0
GLOBAL tByteWidth07.rr
CONST 1
LDIC
GLOBAL Out.Int
CALL 2
!   Out.Ln
GLOBAL Out.Ln
CALL 0
RETURN
END

! Global variables
GLOVAR tByteWidth07.rr 2

! Descriptor for rec
DEFINE tByteWidth07.rec
WORD 0
WORD 0
WORD tByteWidth07.rec.%anc

DEFINE tByteWidth07.rec.%anc
WORD tByteWidth07.rec

! End of file
]]*)
