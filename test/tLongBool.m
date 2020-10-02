MODULE tLongBool;

(* LONGINT comparisons with Boolean result *)

IMPORT Out;

VAR x: LONGINT; b: BOOLEAN;

BEGIN
  x := 12345678912345678;
  b := (x < x+1);
  IF b THEN Out.String("OK"); Out.Ln END;
  IF x+3 > 10 THEN Out.String("OK"); Out.Ln END;
  b := (x+4 > 11);
  IF b THEN Out.String("OK"); Out.Ln END
END tLongBool.

(*<<
OK
OK
OK
>>*)

(*[[
!! (SYMFILE #tLongBool STAMP #tLongBool.%main 1 #tLongBool.m)
!! (CHKSUM STAMP)
!! 
MODULE tLongBool STAMP 0
IMPORT Out STAMP
ENDHDR

PROC tLongBool.%main 0 6 0
!   x := 12345678912345678;
QCONST 12345678912345678
STGQ tLongBool.x
!   b := (x < x+1);
LDGQ tLongBool.x
LDGQ tLongBool.x
CONST 1
CONVNQ
QPLUS
QLT
STGC tLongBool.b
!   IF b THEN Out.String("OK"); Out.Ln END;
LDGC tLongBool.b
JEQZ L4
CONST 3
GLOBAL tLongBool.%1
GLOBAL Out.String
CALL 2
GLOBAL Out.Ln
CALL 0
LABEL L4
!   IF x+3 > 10 THEN Out.String("OK"); Out.Ln END;
LDGQ tLongBool.x
CONST 3
CONVNQ
QPLUS
CONST 10
CONVNQ
QJLEQ L7
CONST 3
GLOBAL tLongBool.%1
GLOBAL Out.String
CALL 2
GLOBAL Out.Ln
CALL 0
LABEL L7
!   b := (x+4 > 11);
LDGQ tLongBool.x
CONST 4
CONVNQ
QPLUS
CONST 11
CONVNQ
QGT
STGC tLongBool.b
!   IF b THEN Out.String("OK"); Out.Ln END
LDGC tLongBool.b
JEQZ L10
CONST 3
GLOBAL tLongBool.%1
GLOBAL Out.String
CALL 2
GLOBAL Out.Ln
CALL 0
LABEL L10
RETURN
END

! Global variables
GLOVAR tLongBool.x 8
GLOVAR tLongBool.b 1

! String "OK"
DEFINE tLongBool.%1
STRING 4F4B00

! End of file
]]*)
