MODULE tSamson;
                        
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
END tSamson.

(*[[
!! (SYMFILE #tSamson STAMP #tSamson.%main 1 #tSamson.m)
!! (CHKSUM STAMP)
!! 
MODULE tSamson STAMP 0
IMPORT Out STAMP
ENDHDR

PROC tSamson.%main 0 4 0
!   NEW(b);
CONST 4
GLOBAL tSamson.BDesc
GLOBAL NEW
CALLW 2
STGW tSamson.b
!   b^.s := 3;
CONST 3
LDGW tSamson.b
NCHECK 20
STOREW
!   a := b;
LDGW tSamson.b
STGW tSamson.a
!   IF a(B)^.s = 3 THEN
LDGW tSamson.a
DUP 0
NCHECK 23
LDNW -4
DUP 0
LDNW 4
CONST 1
JGEQ L8
POP 1
JUMP L7
LABEL L8
LDNW 8
LDNW 4
GLOBAL tSamson.BDesc
JEQ L6
LABEL L7
ERROR E_CAST 23
LABEL L6
LOADW
CONST 3
JNEQ L5
!     Out.String("OK");
CONST 3
GLOBAL tSamson.%1
GLOBAL Out.String
CALL 2
JUMP L3
LABEL L5
!     Out.String("EROOR");
CONST 6
GLOBAL tSamson.%2
GLOBAL Out.String
CALL 2
LABEL L3
!   Out.Ln()
GLOBAL Out.Ln
CALL 0
RETURN
END

! Global variables
GLOVAR tSamson.a 4
GLOVAR tSamson.b 4

! Global pointer map
DEFINE tSamson.%gcmap
WORD GC_POINTER
WORD tSamson.a
WORD GC_POINTER
WORD tSamson.b
WORD GC_END

! String "OK"
DEFINE tSamson.%1
STRING 4F4B00

! String "EROOR"
DEFINE tSamson.%2
STRING 45524F4F5200

! Descriptor for ADesc
DEFINE tSamson.ADesc
WORD 0
WORD 0
WORD tSamson.ADesc.%anc

DEFINE tSamson.ADesc.%anc
WORD tSamson.ADesc

! Descriptor for BDesc
DEFINE tSamson.BDesc
WORD 0
WORD 1
WORD tSamson.BDesc.%anc

DEFINE tSamson.BDesc.%anc
WORD tSamson.ADesc
WORD tSamson.BDesc

! End of file
]]*)
