MODULE tSetConst07;

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
END tSetConst07.

(*<<
True
>>*)

(*[[
!! (SYMFILE #tSetConst07 STAMP #tSetConst07.%main 1 #tSetConst07.m)
!! (CHKSUM STAMP)
!! 
MODULE tSetConst07 STAMP 0
IMPORT Out STAMP
ENDHDR

PROC tSetConst07.%main 0 3 0
!         Out.String("True");
CONST 5
GLOBAL tSetConst07.%1
GLOBAL Out.String
CALL 2
!     Out.Ln
GLOBAL Out.Ln
CALL 0
RETURN
END

! String "True"
DEFINE tSetConst07.%1
STRING 5472756500

! String "False"
DEFINE tSetConst07.%2
STRING 46616C736500

! End of file
]]*)
