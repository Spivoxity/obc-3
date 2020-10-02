MODULE tSuper2;

(*<<
r1.Foo
r1.Foo
r3.Foo
s1.Foo
s1.Foo
s3.Foo
>>*)

IMPORT xPrelude, Out;

TYPE 
  r1 = RECORD END;
  r2 = RECORD (r1) END;
  r3 = RECORD (r2) END;
  s3 = RECORD (xPrelude.s2) END;

PROCEDURE (VAR x: r1) Foo; 
BEGIN Out.String("r1.Foo"); Out.Ln END Foo;

PROCEDURE (VAR x: r3) Foo; 
BEGIN x.Foo^; Out.String("r3.Foo"); Out.Ln END Foo;

PROCEDURE (VAR x: s3) Foo*; 
BEGIN x.Foo^; Out.String("s3.Foo"); Out.Ln END Foo;

PROCEDURE (VAR x: s3) Baz*; END Baz;

VAR x: r3; y: s3;

BEGIN
  (* This is ok *)
  x.Foo^; 
  x.Foo;

  (* But what about this? *)
  y.Foo^; 
  y.Foo;
END tSuper2.

(*[[
!! (SYMFILE #tSuper2 STAMP #tSuper2.%main 1 #tSuper2.m)
!! (CHKSUM STAMP)
!! 
MODULE tSuper2 STAMP 0
IMPORT xPrelude STAMP
IMPORT Out STAMP
ENDHDR

PROC tSuper2.r1.Foo 0 3 0x00100001
! PROCEDURE (VAR x: r1) Foo; 
! BEGIN Out.String("r1.Foo"); Out.Ln END Foo;
CONST 7
GLOBAL tSuper2.%1
GLOBAL Out.String
CALL 2
GLOBAL Out.Ln
CALL 0
RETURN
END

PROC tSuper2.r3.Foo 0 3 0x00100001
! PROCEDURE (VAR x: r3) Foo; 
! BEGIN x.Foo^; Out.String("r3.Foo"); Out.Ln END Foo;
LDLW 16
LDLW 12
GLOBAL tSuper2.r1.Foo
CALL 2
CONST 7
GLOBAL tSuper2.%2
GLOBAL Out.String
CALL 2
GLOBAL Out.Ln
CALL 0
RETURN
END

PROC tSuper2.s3.Foo 0 3 0x00100001
! PROCEDURE (VAR x: s3) Foo*; 
! BEGIN x.Foo^; Out.String("s3.Foo"); Out.Ln END Foo;
LDLW 16
LDLW 12
GLOBAL xPrelude.s1.Foo
CALL 2
CONST 7
GLOBAL tSuper2.%3
GLOBAL Out.String
CALL 2
GLOBAL Out.Ln
CALL 0
RETURN
END

PROC tSuper2.s3.Baz 0 0 0x00100001
! PROCEDURE (VAR x: s3) Baz*; END Baz;
RETURN
END

PROC tSuper2.%main 0 3 0
!   x.Foo^; 
GLOBAL tSuper2.r3
GLOBAL tSuper2.x
GLOBAL tSuper2.r1.Foo
CALL 2
!   x.Foo;
GLOBAL tSuper2.r3
GLOBAL tSuper2.x
GLOBAL tSuper2.r3.Foo
CALL 2
!   y.Foo^; 
GLOBAL tSuper2.s3
GLOBAL tSuper2.y
GLOBAL xPrelude.s1.Foo
CALL 2
!   y.Foo;
GLOBAL tSuper2.s3
GLOBAL tSuper2.y
GLOBAL tSuper2.s3.Foo
CALL 2
RETURN
END

! Global variables
GLOVAR tSuper2.x 0
GLOVAR tSuper2.y 0

! String "r1.Foo"
DEFINE tSuper2.%1
STRING 72312E466F6F00

! String "r3.Foo"
DEFINE tSuper2.%2
STRING 72332E466F6F00

! String "s3.Foo"
DEFINE tSuper2.%3
STRING 73332E466F6F00

! Descriptor for r1
DEFINE tSuper2.r1
WORD 0
WORD 0
WORD tSuper2.r1.%anc
WORD tSuper2.r1.Foo

DEFINE tSuper2.r1.%anc
WORD tSuper2.r1

! Descriptor for r2
DEFINE tSuper2.r2
WORD 0
WORD 1
WORD tSuper2.r2.%anc
WORD tSuper2.r1.Foo

DEFINE tSuper2.r2.%anc
WORD tSuper2.r1
WORD tSuper2.r2

! Descriptor for r3
DEFINE tSuper2.r3
WORD 0
WORD 2
WORD tSuper2.r3.%anc
WORD tSuper2.r3.Foo

DEFINE tSuper2.r3.%anc
WORD tSuper2.r1
WORD tSuper2.r2
WORD tSuper2.r3

! Descriptor for s3
DEFINE tSuper2.s3
WORD 0
WORD 2
WORD tSuper2.s3.%anc
WORD tSuper2.s3.Foo
WORD tSuper2.s3.Baz

DEFINE tSuper2.s3.%anc
WORD xPrelude.s1
WORD xPrelude.s2
WORD tSuper2.s3

! End of file
]]*)
