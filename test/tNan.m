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
  Out.Ln
END tNan.

(*<<
Unequal
01
>>*)

(*[[
!! (SYMFILE #tNan STAMP #tNan.%main 1)
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
FJEQ L4
!     Out.String("Unequal"); Out.Ln
CONST 8
GLOBAL tNan.%1
GLOBAL Out.String
CALL 2
GLOBAL Out.Ln
CALL 0
LABEL L4
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

! End of file
]]*)
