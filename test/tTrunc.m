MODULE tTrunc;
(* Alex Shiryaev: effect of CSE on shorts *)

        IMPORT Out;

        VAR x: SHORTINT; y: INTEGER;

BEGIN
        x := 32767;
        INC(x);
	Out.Int(x, 0); Out.Ln;
        ASSERT( x = -32768 ); (* here *)

	y := 50000; x := SHORT(y);
	Out.Int(x, 0); Out.Ln;
END tTrunc.

(*<<
-32768
-15536
>>*)

(*[[
!! (SYMFILE #tTrunc STAMP #tTrunc.%main 1 #tTrunc.m)
!! (CHKSUM STAMP)
!! 
MODULE tTrunc STAMP 0
IMPORT Out STAMP
ENDHDR

PROC tTrunc.%main 0 3 0
!         x := 32767;
CONST 32767
STGS tTrunc.x
!         INC(x);
LDGS tTrunc.x
INC
STGS tTrunc.x
! 	Out.Int(x, 0); Out.Ln;
CONST 0
LDGS tTrunc.x
GLOBAL Out.Int
CALL 2
GLOBAL Out.Ln
CALL 0
!         ASSERT( x = -32768 ); (* here *)
LDGS tTrunc.x
CONST -32768
JEQ L2
CONST 0
CONST 12
GLOBAL EASSERT
CALL 2
LABEL L2
! 	y := 50000; x := SHORT(y);
CONST 50000
STGW tTrunc.y
LDGW tTrunc.y
STGS tTrunc.x
! 	Out.Int(x, 0); Out.Ln;
CONST 0
LDGS tTrunc.x
GLOBAL Out.Int
CALL 2
GLOBAL Out.Ln
CALL 0
RETURN
END

! Global variables
GLOVAR tTrunc.x 2
GLOVAR tTrunc.y 4

! End of file
]]*)
