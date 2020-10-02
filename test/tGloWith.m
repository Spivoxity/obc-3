MODULE tGloWith;

IMPORT Out;

TYPE A = POINTER TO AA; B = POINTER TO BB; C = POINTER TO CC;
  AA = RECORD END;
  BB = RECORD (AA) x: INTEGER; END;
  CC = RECORD (AA) END;

VAR a: A; b: B; c: C;

BEGIN
  NEW(b); b.x := 37; a := b;
  WITH a: B DO Out.Int(a.x, 0) END; 
  Out.Ln;

  NEW(c); a := c;
  WITH a: B DO Out.Int(a.x, 0) ELSE Out.String("none") END;
  Out.Ln
END tGloWith.

(*<<
37
none
>>*)

(*[[
!! (SYMFILE #tGloWith STAMP #tGloWith.%main 1 #tGloWith.m)
!! (CHKSUM STAMP)
!! 
MODULE tGloWith STAMP 0
IMPORT Out STAMP
ENDHDR

PROC tGloWith.%main 0 3 0
!   NEW(b); b.x := 37; a := b;
CONST 4
GLOBAL tGloWith.BB
GLOBAL NEW
CALLW 2
STGW tGloWith.b
CONST 37
LDGW tGloWith.b
NCHECK 13
STOREW
LDGW tGloWith.b
STGW tGloWith.a
!   WITH a: B DO Out.Int(a.x, 0) END; 
LDGW tGloWith.a
NCHECK 14
LDNW -4
DUP 0
LDNW 4
CONST 1
JGEQ L5
POP 1
JUMP L3
LABEL L5
LDNW 8
LDNW 4
GLOBAL tGloWith.BB
JNEQ L3
CONST 0
LDGW tGloWith.a
NCHECK 14
LOADW
GLOBAL Out.Int
CALL 2
JUMP L2
LABEL L3
ERROR E_WITH 14
LABEL L2
!   Out.Ln;
GLOBAL Out.Ln
CALL 0
!   NEW(c); a := c;
CONST 0
GLOBAL tGloWith.CC
GLOBAL NEW
CALLW 2
STGW tGloWith.c
LDGW tGloWith.c
STGW tGloWith.a
!   WITH a: B DO Out.Int(a.x, 0) ELSE Out.String("none") END;
LDGW tGloWith.a
NCHECK 18
LDNW -4
DUP 0
LDNW 4
CONST 1
JGEQ L9
POP 1
JUMP L7
LABEL L9
LDNW 8
LDNW 4
GLOBAL tGloWith.BB
JNEQ L7
CONST 0
LDGW tGloWith.a
NCHECK 18
LOADW
GLOBAL Out.Int
CALL 2
JUMP L6
LABEL L7
CONST 5
GLOBAL tGloWith.%1
GLOBAL Out.String
CALL 2
LABEL L6
!   Out.Ln
GLOBAL Out.Ln
CALL 0
RETURN
END

! Global variables
GLOVAR tGloWith.a 4
GLOVAR tGloWith.b 4
GLOVAR tGloWith.c 4

! Global pointer map
DEFINE tGloWith.%gcmap
WORD GC_POINTER
WORD tGloWith.a
WORD GC_POINTER
WORD tGloWith.b
WORD GC_POINTER
WORD tGloWith.c
WORD GC_END

! String "none"
DEFINE tGloWith.%1
STRING 6E6F6E6500

! Descriptor for AA
DEFINE tGloWith.AA
WORD 0
WORD 0
WORD tGloWith.AA.%anc

DEFINE tGloWith.AA.%anc
WORD tGloWith.AA

! Descriptor for BB
DEFINE tGloWith.BB
WORD 0
WORD 1
WORD tGloWith.BB.%anc

DEFINE tGloWith.BB.%anc
WORD tGloWith.AA
WORD tGloWith.BB

! Descriptor for CC
DEFINE tGloWith.CC
WORD 0
WORD 1
WORD tGloWith.CC.%anc

DEFINE tGloWith.CC.%anc
WORD tGloWith.AA
WORD tGloWith.CC

! End of file
]]*)
