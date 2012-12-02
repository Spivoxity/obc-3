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
!! SYMFILE #tSuper3 STAMP #tSuper3.%main 1
!! END STAMP
!! 
MODULE tSuper3 STAMP 0
IMPORT Out STAMP
ENDHDR

PROC tSuper3.b.f 0 12 0x00100001
! PROCEDURE (p: a) f; BEGIN Out.String("foo"); Out.Ln END f;
CONST 4
CONST tSuper3.%1
CONST Out.String
CALL 2
CONST Out.Ln
CALL 0
RETURN
END

PROC tSuper3.%main 0 16 0
!   NEW(p); p.f^ 
CONST 0
CONST tSuper3.d
CONST tSuper3.p
CONST NEW
CALL 3
LDGW tSuper3.p
CONST tSuper3.b.f
CALL 1
RETURN
END

! Global variables
GLOBAL tSuper3.p 4

! Pointer map
DEFINE tSuper3.%gcmap
WORD GC_BASE
WORD tSuper3.p
WORD 0
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
