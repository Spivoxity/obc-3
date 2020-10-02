MODULE tSuper3;

IMPORT Out;

TYPE a = POINTER TO b; b = RECORD END; 
  c = POINTER TO d; d = RECORD (b) END;

PROCEDURE (p: a) f; BEGIN Out.String("foo"); Out.Ln END f;

VAR p: c;

BEGIN 
  NEW(p); p.f^ 
END tSuper3.

(*<<
foo
>>*)

(*[[
!! (SYMFILE #tSuper3 STAMP #tSuper3.%main 1 #tSuper3.m)
!! (CHKSUM STAMP)
!! 
MODULE tSuper3 STAMP 0
IMPORT Out STAMP
ENDHDR

PROC tSuper3.b.f 0 3 0x00100001
! PROCEDURE (p: a) f; BEGIN Out.String("foo"); Out.Ln END f;
CONST 4
GLOBAL tSuper3.%1
GLOBAL Out.String
CALL 2
GLOBAL Out.Ln
CALL 0
RETURN
END

PROC tSuper3.%main 0 3 0
!   NEW(p); p.f^ 
CONST 0
GLOBAL tSuper3.d
GLOBAL NEW
CALLW 2
STGW tSuper3.p
LDGW tSuper3.p
GLOBAL tSuper3.b.f
CALL 1
RETURN
END

! Global variables
GLOVAR tSuper3.p 4

! Global pointer map
DEFINE tSuper3.%gcmap
WORD GC_POINTER
WORD tSuper3.p
WORD GC_END

! String "foo"
DEFINE tSuper3.%1
STRING 666F6F00

! Descriptor for b
DEFINE tSuper3.b
WORD 0
WORD 0
WORD tSuper3.b.%anc
WORD tSuper3.b.f

DEFINE tSuper3.b.%anc
WORD tSuper3.b

! Descriptor for d
DEFINE tSuper3.d
WORD 0
WORD 1
WORD tSuper3.d.%anc
WORD tSuper3.b.f

DEFINE tSuper3.d.%anc
WORD tSuper3.b
WORD tSuper3.d

! End of file
]]*)
