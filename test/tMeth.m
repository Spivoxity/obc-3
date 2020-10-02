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

PROCEDURE Main;
VAR it: foo;
BEGIN
  it.doit
END Main;

VAR p: POINTER TO foo;

BEGIN
  Main;
  NEW(p); p.doit
END tMeth.

(*[[
!! (SYMFILE #tMeth STAMP #tMeth.%main 1 #tMeth.m)
!! (CHKSUM STAMP)
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

PROC tMeth.Main 0 3 0
! PROCEDURE Main;
!   it.doit
GLOBAL tMeth.foo
LOCAL 0
GLOBAL tMeth.foo.doit
CALL 2
RETURN
END

PROC tMeth.%main 0 4 0
!   Main;
GLOBAL tMeth.Main
CALL 0
!   NEW(p); p.doit
CONST 0
GLOBAL tMeth.foo
GLOBAL NEW
CALLW 2
STGW tMeth.p
LDGW tMeth.p
NCHECK 34
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

! Global pointer map
DEFINE tMeth.%gcmap
WORD GC_POINTER
WORD tMeth.p
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
