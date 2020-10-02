MODULE tAbsImport;

IMPORT X := xTypes, Out;

TYPE Thang = POINTER TO Blip;
  Blip = RECORD (X.Blob) END;

PROCEDURE MkBlip(): X.Thing;
  VAR p: Thang;
BEGIN
  NEW(p); RETURN p
END MkBlip;

PROCEDURE (p: Thang) Print*; BEGIN Out.String("A Blip") END Print;

VAR p: X.Thing;

BEGIN
  p := MkBlip();
  p.Print; Out.Ln
END tAbsImport.

(*<<
A Blip
>>*)

(*[[
!! (SYMFILE #tAbsImport STAMP #tAbsImport.%main 1 #tAbsImport.m)
!! (CHKSUM STAMP)
!! 
MODULE tAbsImport STAMP 0
IMPORT xTypes STAMP
IMPORT Out STAMP
ENDHDR

PROC tAbsImport.MkBlip 4 3 0x00010001
! PROCEDURE MkBlip(): X.Thing;
!   NEW(p); RETURN p
CONST 0
GLOBAL tAbsImport.Blip
GLOBAL NEW
CALLW 2
STLW -4
LDLW -4
RETURN
END

PROC tAbsImport.Blip.Print 0 3 0x00100001
! PROCEDURE (p: Thang) Print*; BEGIN Out.String("A Blip") END Print;
CONST 7
GLOBAL tAbsImport.%1
GLOBAL Out.String
CALL 2
RETURN
END

PROC tAbsImport.%main 0 3 0
!   p := MkBlip();
GLOBAL tAbsImport.MkBlip
CALLW 0
STGW tAbsImport.p
!   p.Print; Out.Ln
LDGW tAbsImport.p
NCHECK 20
DUP 0
LDNW -4
LDNW 12
CALL 1
GLOBAL Out.Ln
CALL 0
RETURN
END

! Global variables
GLOVAR tAbsImport.p 4

! Global pointer map
DEFINE tAbsImport.%gcmap
WORD GC_POINTER
WORD tAbsImport.p
WORD GC_END

! String "A Blip"
DEFINE tAbsImport.%1
STRING 4120426C697000

! Descriptor for Blip
DEFINE tAbsImport.Blip
WORD 0
WORD 1
WORD tAbsImport.Blip.%anc
WORD tAbsImport.Blip.Print

DEFINE tAbsImport.Blip.%anc
WORD xTypes.Blob
WORD tAbsImport.Blip

! End of file
]]*)
