MODULE tNan;

IMPORT Out;

VAR nan: REAL; b: BOOLEAN;

BEGIN
  nan := 0.0;
  nan := nan/nan;
  (* Out.Real(nan); Out.Ln; -- results differ, NAN vs -NAN *)
  IF nan # nan THEN
    Out.String("Unequal"); Out.Ln
  END;

  b := (nan = nan); Out.Int(ORD(b), 0);
  b := (nan # nan); Out.Int(ORD(b), 0);
  Out.Ln;

  IF nan < 0 THEN Out.String("F1") END;
  IF nan <= 0 THEN Out.String("F2") END;
  IF nan = 0 THEN Out.String("F3") END;
  IF nan >= 0 THEN Out.String("F4") END;
  IF nan > 0 THEN Out.String("F5") END;
  IF ~ (nan < 0) THEN Out.String("S1") END;
  IF ~ (nan <= 0) THEN Out.String("S2") END;
  IF ~ (nan = 0) THEN Out.String("S3") END;
  IF ~ (nan >= 0) THEN Out.String("S4") END;
  IF ~ (nan > 0) THEN Out.String("S5") END;
  Out.Ln
END tNan.

(*<<
Unequal
01
S1S2S3S4S5
>>*)

(*[[
!! (SYMFILE #tNan STAMP #tNan.%main 1 #tNan.m)
!! (CHKSUM STAMP)
!! 
MODULE tNan STAMP 0
IMPORT Out STAMP
ENDHDR

PROC tNan.%main 0 3 0
!   nan := 0.0;
FCONST 0.0
STGF tNan.nan
!   nan := nan/nan;
LDGF tNan.nan
LDGF tNan.nan
FDIV
STGF tNan.nan
!   IF nan # nan THEN
LDGF tNan.nan
LDGF tNan.nan
FJEQ L14
!     Out.String("Unequal"); Out.Ln
CONST 8
GLOBAL tNan.%1
GLOBAL Out.String
CALL 2
GLOBAL Out.Ln
CALL 0
LABEL L14
!   b := (nan = nan); Out.Int(ORD(b), 0);
LDGF tNan.nan
LDGF tNan.nan
FEQ
STGC tNan.b
CONST 0
LDGC tNan.b
GLOBAL Out.Int
CALL 2
!   b := (nan # nan); Out.Int(ORD(b), 0);
LDGF tNan.nan
LDGF tNan.nan
FNEQ
STGC tNan.b
CONST 0
LDGC tNan.b
GLOBAL Out.Int
CALL 2
!   Out.Ln;
GLOBAL Out.Ln
CALL 0
!   IF nan < 0 THEN Out.String("F1") END;
LDGF tNan.nan
FCONST 0.0
FJNLT L17
CONST 3
GLOBAL tNan.%2
GLOBAL Out.String
CALL 2
LABEL L17
!   IF nan <= 0 THEN Out.String("F2") END;
LDGF tNan.nan
FCONST 0.0
FJNLEQ L20
CONST 3
GLOBAL tNan.%3
GLOBAL Out.String
CALL 2
LABEL L20
!   IF nan = 0 THEN Out.String("F3") END;
LDGF tNan.nan
FCONST 0.0
FJNEQ L23
CONST 3
GLOBAL tNan.%4
GLOBAL Out.String
CALL 2
LABEL L23
!   IF nan >= 0 THEN Out.String("F4") END;
LDGF tNan.nan
FCONST 0.0
FJNGEQ L26
CONST 3
GLOBAL tNan.%5
GLOBAL Out.String
CALL 2
LABEL L26
!   IF nan > 0 THEN Out.String("F5") END;
LDGF tNan.nan
FCONST 0.0
FJNGT L29
CONST 3
GLOBAL tNan.%6
GLOBAL Out.String
CALL 2
LABEL L29
!   IF ~ (nan < 0) THEN Out.String("S1") END;
LDGF tNan.nan
FCONST 0.0
FJLT L32
CONST 3
GLOBAL tNan.%7
GLOBAL Out.String
CALL 2
LABEL L32
!   IF ~ (nan <= 0) THEN Out.String("S2") END;
LDGF tNan.nan
FCONST 0.0
FJLEQ L35
CONST 3
GLOBAL tNan.%8
GLOBAL Out.String
CALL 2
LABEL L35
!   IF ~ (nan = 0) THEN Out.String("S3") END;
LDGF tNan.nan
FCONST 0.0
FJEQ L38
CONST 3
GLOBAL tNan.%9
GLOBAL Out.String
CALL 2
LABEL L38
!   IF ~ (nan >= 0) THEN Out.String("S4") END;
LDGF tNan.nan
FCONST 0.0
FJGEQ L41
CONST 3
GLOBAL tNan.%10
GLOBAL Out.String
CALL 2
LABEL L41
!   IF ~ (nan > 0) THEN Out.String("S5") END;
LDGF tNan.nan
FCONST 0.0
FJGT L44
CONST 3
GLOBAL tNan.%11
GLOBAL Out.String
CALL 2
LABEL L44
!   Out.Ln
GLOBAL Out.Ln
CALL 0
RETURN
END

! Global variables
GLOVAR tNan.nan 4
GLOVAR tNan.b 1

! String "Unequal"
DEFINE tNan.%1
STRING 556E657175616C00

! String "F1"
DEFINE tNan.%2
STRING 463100

! String "F2"
DEFINE tNan.%3
STRING 463200

! String "F3"
DEFINE tNan.%4
STRING 463300

! String "F4"
DEFINE tNan.%5
STRING 463400

! String "F5"
DEFINE tNan.%6
STRING 463500

! String "S1"
DEFINE tNan.%7
STRING 533100

! String "S2"
DEFINE tNan.%8
STRING 533200

! String "S3"
DEFINE tNan.%9
STRING 533300

! String "S4"
DEFINE tNan.%10
STRING 533400

! String "S5"
DEFINE tNan.%11
STRING 533500

! End of file
]]*)
