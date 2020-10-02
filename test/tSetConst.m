MODULE tSetConst;

(* Michael Bradley 24/1/11 *)

IMPORT Out;

CONST x = {1,2,3};

BEGIN
    IF (1 IN x) THEN
        Out.String("True");
    ELSE
        Out.String("False");
    END;
    Out.Ln
END tSetConst.

(*<<
True
>>*)

(*[[
!! (SYMFILE #tSetConst STAMP #tSetConst.%main 1 #tSetConst.m)
!! (CHKSUM STAMP)
!! 
MODULE tSetConst STAMP 0
IMPORT Out STAMP
ENDHDR

PROC tSetConst.%main 0 3 0
!         Out.String("True");
CONST 5
GLOBAL tSetConst.%1
GLOBAL Out.String
CALL 2
!     Out.Ln
GLOBAL Out.Ln
CALL 0
RETURN
END

! String "True"
DEFINE tSetConst.%1
STRING 5472756500

! String "False"
DEFINE tSetConst.%2
STRING 46616C736500

! End of file
]]*)
