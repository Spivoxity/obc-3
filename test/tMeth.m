MODULE tMeth;

(* A basic test that dynamic method call works properly *)

(*<<
Hello!
Hello!
>>*)

IMPORT Out;

TYPE foo = RECORD END;

PROCEDURE (VAR f: foo) baz; 
BEGIN 
  Out.String("Hello!"); Out.Ln 
END baz;

PROCEDURE (VAR f: foo) doit;
BEGIN
  f.baz
END doit;

VAR p: POINTER TO foo;

BEGIN
  VAR it: foo; BEGIN it.doit END;
  NEW(p); p.doit
END tMeth.

(*[[
!! SYMFILE #tMeth STAMP #tMeth.%main 1
!! END STAMP
!! 
MODULE tMeth STAMP 0
IMPORT Out STAMP
ENDHDR

PROC tMeth.foo.baz 0 3 0x00100001
! PROCEDURE (VAR f: foo) baz; 
!   Out.String("Hello!"); Out.Ln 
CONST 7
GLOBAL tMeth.%1
GLOBAL Out.String
CALL 2
GLOBAL Out.Ln
CALL 0
RETURN
END

PROC tMeth.foo.doit 0 4 0x00100001
! PROCEDURE (VAR f: foo) doit;
!   f.baz
LDLW 16
LDLW 12
DUP 1
LDNW 12
CALL 2
RETURN
END

PROC tMeth.%main 0 4 0
!   VAR it: foo; BEGIN it.doit END;
GLOBAL tMeth.foo
LOCAL 0
GLOBAL tMeth.foo.doit
CALL 2
!   NEW(p); p.doit
CONST 0
GLOBAL tMeth.foo
GLOBAL tMeth.p
GLOBAL NEW
CALL 3
LDGW tMeth.p
NCHECK 28
DUP 0
LDNW -4
SWAP
DUP 1
LDNW 16
CALL 2
RETURN
END

! Global variables
GLOVAR tMeth.p 4

! Pointer map
DEFINE tMeth.%gcmap
WORD GC_BASE
WORD tMeth.p
WORD 0
WORD GC_END

! String "Hello!"
DEFINE tMeth.%1
STRING 48656C6C6F2100

! Descriptor for foo
DEFINE tMeth.foo
WORD 0
WORD 0
WORD tMeth.foo.%anc
WORD tMeth.foo.baz
WORD tMeth.foo.doit

DEFINE tMeth.foo.%anc
WORD tMeth.foo

! End of file
]]*)
