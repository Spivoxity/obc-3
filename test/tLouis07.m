MODULE tLouis07;

(* Issue #20: use of qualfied names as labels in typecase *)

IMPORT xTypes07, Out;

PROCEDURE b(x: xTypes07.Base);
BEGIN
  CASE x OF
      xTypes07.Ext1:
        Out.String("One "); Out.Int(x.stuff1, 0); Out.Ln
    | xTypes07.Ext2:
        Out.String("Two "); Out.Real(x.stuff2); Out.Ln
  END
END b;

PROCEDURE test;
  VAR p1: xTypes07.Ext1; p2: xTypes07.Ext2;
BEGIN
  NEW(p1); p1.stuff1 := 3; b(p1);
  NEW(p2); p2.stuff2 := 3.14; b(p2)
END test;

BEGIN
  test
END tLouis07.

(*<<
One 3
Two 3.14000
>>*)

(*[[
!! SYMFILE #tLouis07 STAMP #tLouis07.%main 1
!! END STAMP
!! 
MODULE tLouis07 STAMP 0
IMPORT xTypes07 STAMP
IMPORT Out STAMP
ENDHDR

PROC tLouis07.b 0 4 0x00100001
! PROCEDURE b(x: xTypes07.Base);
!   CASE x OF
LDLW 12
NCHECK 9
CONST -4
PLUSA
DUP 0
LOADW
DUP 0
LDNW 4
CONST 1
JGEQ L6
POP 1
JUMP L4
LABEL L6
LDNW 8
LDNW 4
GLOBAL xTypes07.Ext1Cell
JNEQ L4
POP 1
!         Out.String("One "); Out.Int(x.stuff1, 0); Out.Ln
CONST 5
GLOBAL tLouis07.%1
GLOBAL Out.String
CALL 2
CONST 0
LDLW 12
NCHECK 11
LOADW
GLOBAL Out.Int
CALL 2
GLOBAL Out.Ln
CALL 0
RETURN
LABEL L4
DUP 0
LOADW
DUP 0
LDNW 4
CONST 1
JGEQ L9
POP 1
JUMP L7
LABEL L9
LDNW 8
LDNW 4
GLOBAL xTypes07.Ext2Cell
JNEQ L7
POP 1
!         Out.String("Two "); Out.Real(x.stuff2); Out.Ln
CONST 5
GLOBAL tLouis07.%2
GLOBAL Out.String
CALL 2
LDLW 12
NCHECK 13
LOADF
GLOBAL Out.Real
CALL 1
GLOBAL Out.Ln
CALL 0
RETURN
LABEL L7
POP 1
ERROR E_CASE 9
RETURN
END

PROC tLouis07.test 8 4 0x00018001
! PROCEDURE test;
!   NEW(p1); p1.stuff1 := 3; b(p1);
CONST 4
GLOBAL xTypes07.Ext1Cell
GLOBAL NEW
CALLW 2
STLW -4
CONST 3
LDLW -4
NCHECK 20
STOREW
LDLW -4
GLOBAL tLouis07.b
CALL 1
!   NEW(p2); p2.stuff2 := 3.14; b(p2)
CONST 4
GLOBAL xTypes07.Ext2Cell
GLOBAL NEW
CALLW 2
STLW -8
FCONST 3.14
LDLW -8
NCHECK 21
STOREF
LDLW -8
GLOBAL tLouis07.b
CALL 1
RETURN
END

PROC tLouis07.%main 0 4 0
!   test
GLOBAL tLouis07.test
CALL 0
RETURN
END

! String "One "
DEFINE tLouis07.%1
STRING 4F6E652000

! String "Two "
DEFINE tLouis07.%2
STRING 54776F2000

! End of file
]]*)
