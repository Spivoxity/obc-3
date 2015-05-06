MODULE tSamson07;
                        
(*<<
OK
>>*)

IMPORT Out;
                        
TYPE
  A = POINTER TO ADesc;
  ADesc = RECORD END;
                                        
  B = POINTER TO BDesc;
  BDesc = RECORD(ADesc) s: INTEGER; END;
                        
 VAR a: A; b: B;
                                
BEGIN
  NEW(b);
  b^.s := 3;
  a := b;
                                
  IF a(B)^.s = 3 THEN
    Out.String("OK");
  ELSE
    Out.String("EROOR");
  END;
  Out.Ln()
END tSamson07.

(*[[
!! SYMFILE #tSamson07 STAMP #tSamson07.%main 1
!! END STAMP
!! 
MODULE tSamson07 STAMP 0
IMPORT Out STAMP
ENDHDR

PROC tSamson07.%main 0 4 0
!   NEW(b);
CONST 4
GLOBAL tSamson07.BDesc
GLOBAL tSamson07.b
GLOBAL NEW
CALL 3
!   b^.s := 3;
CONST 3
LDGW tSamson07.b
NCHECK 20
STOREW
!   a := b;
LDGW tSamson07.b
STGW tSamson07.a
!   IF a(B)^.s = 3 THEN
LDGW tSamson07.a
DUP 0
NCHECK 23
LDNW -4
DUP 0
LDNW 4
CONST 1
JGEQ 8
POP 1
JUMP 7
LABEL 8
LDNW 8
LDNW 4
GLOBAL tSamson07.BDesc
JEQ 6
LABEL 7
ERROR E_CAST 23
LABEL 6
LOADW
CONST 3
JNEQ 5
!     Out.String("OK");
CONST 3
GLOBAL tSamson07.%1
GLOBAL Out.String
CALL 2
JUMP 3
LABEL 5
!     Out.String("EROOR");
CONST 6
GLOBAL tSamson07.%2
GLOBAL Out.String
CALL 2
LABEL 3
!   Out.Ln()
GLOBAL Out.Ln
CALL 0
RETURN
END

! Global variables
GLOVAR tSamson07.a 4
GLOVAR tSamson07.b 4

! Pointer map
DEFINE tSamson07.%gcmap
WORD GC_BASE
WORD tSamson07.a
WORD 0
WORD GC_BASE
WORD tSamson07.b
WORD 0
WORD GC_END

! String "OK"
DEFINE tSamson07.%1
STRING 4F4B00

! String "EROOR"
DEFINE tSamson07.%2
STRING 45524F4F5200

! Descriptor for ADesc
DEFINE tSamson07.ADesc
WORD 0
WORD 0
WORD tSamson07.ADesc.%anc

DEFINE tSamson07.ADesc.%anc
WORD tSamson07.ADesc

! Descriptor for BDesc
DEFINE tSamson07.BDesc
WORD 0
WORD 1
WORD tSamson07.BDesc.%anc

DEFINE tSamson07.BDesc.%anc
WORD tSamson07.ADesc
WORD tSamson07.BDesc

! End of file
]]*)
