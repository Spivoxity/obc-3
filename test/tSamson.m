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
!! SYMFILE #tSamson STAMP #tSamson.%main 1
!! END STAMP
!! 
MODULE tSamson STAMP 0
IMPORT Out STAMP
ENDHDR

PROC tSamson.%main 0 16 0
!   NEW(b);
CONST 4
CONST tSamson.BDesc
CONST tSamson.b
CONST NEW
CALL 3
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
CONST tSamson.BDesc
TYPETEST 1
JUMPT 5
ERROR E_CAST 23
LABEL 5
LOADW
CONST 3
JNEQ 4
!     Out.String("OK");
CONST 3
CONST tSamson.%1
CONST Out.String
CALL 2
JUMP 3
LABEL 4
!     Out.String("EROOR");
CONST 6
CONST tSamson.%2
CONST Out.String
CALL 2
LABEL 3
!   Out.Ln()
CONST Out.Ln
CALL 0
RETURN
END

! Global variables
GLOBAL tSamson.a 4
GLOBAL tSamson.b 4

! Pointer map
DEFINE tSamson.%gcmap
WORD GC_BASE
WORD tSamson.a
WORD 0
WORD GC_BASE
WORD tSamson.b
WORD 0
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
