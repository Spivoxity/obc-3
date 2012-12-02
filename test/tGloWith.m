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
!! SYMFILE #tGloWith STAMP #tGloWith.%main 1
!! END STAMP
!! 
MODULE tGloWith STAMP 0
IMPORT Out STAMP
ENDHDR

PROC tGloWith.%main 0 16 0
!   NEW(b); b.x := 37; a := b;
CONST 4
CONST tGloWith.BB
CONST tGloWith.b
CONST NEW
CALL 3
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
CONST tGloWith.BB
TYPETEST 1
JUMPF 3
CONST 0
LDGW tGloWith.a
NCHECK 14
LOADW
CONST Out.Int
CALL 2
JUMP 2
LABEL 3
ERROR E_WITH 14
LABEL 2
!   Out.Ln;
CONST Out.Ln
CALL 0
!   NEW(c); a := c;
CONST 0
CONST tGloWith.CC
CONST tGloWith.c
CONST NEW
CALL 3
LDGW tGloWith.c
STGW tGloWith.a
!   WITH a: B DO Out.Int(a.x, 0) ELSE Out.String("none") END;
LDGW tGloWith.a
NCHECK 18
LDNW -4
CONST tGloWith.BB
TYPETEST 1
JUMPF 5
CONST 0
LDGW tGloWith.a
NCHECK 18
LOADW
CONST Out.Int
CALL 2
JUMP 4
LABEL 5
CONST 5
CONST tGloWith.%1
CONST Out.String
CALL 2
LABEL 4
!   Out.Ln
CONST Out.Ln
CALL 0
RETURN
END

! Global variables
GLOBAL tGloWith.a 4
GLOBAL tGloWith.b 4
GLOBAL tGloWith.c 4

! Pointer map
DEFINE tGloWith.%gcmap
WORD GC_BASE
WORD tGloWith.a
WORD 0
WORD GC_BASE
WORD tGloWith.b
WORD 0
WORD GC_BASE
WORD tGloWith.c
WORD 0
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
