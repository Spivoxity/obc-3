MODULE tSuper;

(*<<
baz2
foo*2
baz1
foo*1
baz0
foo*0
foo2
foo1
foo0
>>*)

IMPORT Out;

TYPE 
  foo = RECORD END;
  baz = RECORD (foo) END;

PROCEDURE (VAR f: foo) Reprint(i: SHORTINT); 
BEGIN
  f.Print(i-1)
END Reprint;

PROCEDURE (VAR f: foo) Print(i: INTEGER);
  VAR b: BOOLEAN;
BEGIN
  b := (f IS baz);
  Out.String("foo");
  IF b THEN Out.Char('*') END;
  Out.Int(i, 0); Out.Ln;
  IF i > 0 THEN f.Reprint(SHORT(i)) END;
END Print;

PROCEDURE (VAR b: baz) Print(i: INTEGER);
BEGIN
  Out.String("baz"); Out.Int(i, 0); Out.Ln;
  b.Print^(i);
END Print;

PROCEDURE DoPrint(VAR f: foo);
BEGIN
  f.Print(2)
END DoPrint;

PROCEDURE Print2;
  VAR f: foo;
BEGIN
  f.Print(2)
END Print2;

VAR bb: baz;

BEGIN
  DoPrint(bb);

(* Prints baz2 (bb's Print method), foo2 (superclass Print method),
     baz1 (bb's method again), foo1, baz0, foo0 *)

  Print2

END tSuper.

(*[[
!! (SYMFILE #tSuper STAMP #tSuper.%main 1 #tSuper.m)
!! (CHKSUM STAMP)
!! 
MODULE tSuper STAMP 0
IMPORT Out STAMP
ENDHDR

PROC tSuper.foo.Reprint 0 5 0x00100001
! PROCEDURE (VAR f: foo) Reprint(i: SHORTINT); 
!   f.Print(i-1)
LDLS 20
DEC
LDLW 16
LDLW 12
DUP 1
LDNW 16
CALL 3
RETURN
END

PROC tSuper.foo.Print 4 5 0x00100001
! PROCEDURE (VAR f: foo) Print(i: INTEGER);
!   b := (f IS baz);
LDLW 16
DUP 0
LDNW 4
CONST 1
JGEQ L6
POP 1
JUMP L4
LABEL L6
LDNW 8
LDNW 4
GLOBAL tSuper.baz
JNEQ L4
CONST 1
JUMP L5
LABEL L4
CONST 0
LABEL L5
STLC -1
!   Out.String("foo");
CONST 4
GLOBAL tSuper.%1
GLOBAL Out.String
CALL 2
!   IF b THEN Out.Char('*') END;
LDLC -1
JEQZ L9
CONST 42
ALIGNC
GLOBAL Out.Char
CALL 1
LABEL L9
!   Out.Int(i, 0); Out.Ln;
CONST 0
LDLW 20
GLOBAL Out.Int
CALL 2
GLOBAL Out.Ln
CALL 0
!   IF i > 0 THEN f.Reprint(SHORT(i)) END;
LDLW 20
JLEQZ L12
LDLW 20
CONVNS
ALIGNS
LDLW 16
LDLW 12
DUP 1
LDNW 12
CALL 3
LABEL L12
RETURN
END

PROC tSuper.baz.Print 0 4 0x00100001
! PROCEDURE (VAR b: baz) Print(i: INTEGER);
!   Out.String("baz"); Out.Int(i, 0); Out.Ln;
CONST 4
GLOBAL tSuper.%2
GLOBAL Out.String
CALL 2
CONST 0
LDLW 20
GLOBAL Out.Int
CALL 2
GLOBAL Out.Ln
CALL 0
!   b.Print^(i);
LDLW 20
LDLW 16
LDLW 12
GLOBAL tSuper.foo.Print
CALL 3
RETURN
END

PROC tSuper.DoPrint 0 5 0x00100001
! PROCEDURE DoPrint(VAR f: foo);
!   f.Print(2)
CONST 2
LDLW 16
LDLW 12
DUP 1
LDNW 16
CALL 3
RETURN
END

PROC tSuper.Print2 0 4 0
! PROCEDURE Print2;
!   f.Print(2)
CONST 2
GLOBAL tSuper.foo
LOCAL 0
GLOBAL tSuper.foo.Print
CALL 3
RETURN
END

PROC tSuper.%main 0 3 0
!   DoPrint(bb);
GLOBAL tSuper.baz
GLOBAL tSuper.bb
GLOBAL tSuper.DoPrint
CALL 2
!   Print2
GLOBAL tSuper.Print2
CALL 0
RETURN
END

! Global variables
GLOVAR tSuper.bb 0

! String "foo"
DEFINE tSuper.%1
STRING 666F6F00

! String "baz"
DEFINE tSuper.%2
STRING 62617A00

! Descriptor for foo
DEFINE tSuper.foo
WORD 0
WORD 0
WORD tSuper.foo.%anc
WORD tSuper.foo.Reprint
WORD tSuper.foo.Print

DEFINE tSuper.foo.%anc
WORD tSuper.foo

! Descriptor for baz
DEFINE tSuper.baz
WORD 0
WORD 1
WORD tSuper.baz.%anc
WORD tSuper.foo.Reprint
WORD tSuper.baz.Print

DEFINE tSuper.baz.%anc
WORD tSuper.foo
WORD tSuper.baz

! End of file
]]*)
