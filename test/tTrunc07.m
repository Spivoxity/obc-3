MODULE tTrunc07;
(* Alex Shiryaev: effect of CSE on shorts *)

        IMPORT Out;

        VAR x: SHORTINT; y: INTEGER;

BEGIN
        x := 32767;
        INC(x);
	Out.Int(x, 0); Out.Ln;
        ASSERT( x = -32768 ); (* here *)

	y := 50000; x := y;
	Out.Int(x, 0); Out.Ln;
END tTrunc07.

(*<<
-32768
-15536
>>*)

(*[[
!! (SYMFILE #tTrunc07 STAMP #tTrunc07.%main 1 #tTrunc07.m)
!! (CHKSUM STAMP)
!! 
MODULE tTrunc07 STAMP 0
IMPORT Out STAMP
ENDHDR

PROC tTrunc07.%main 0 3 0
!         x := 32767;
CONST 32767
STGS tTrunc07.x
!         INC(x);
LDGS tTrunc07.x
INC
STGS tTrunc07.x
! 	Out.Int(x, 0); Out.Ln;
CONST 0
LDGS tTrunc07.x
GLOBAL Out.Int
CALL 2
GLOBAL Out.Ln
CALL 0
!         ASSERT( x = -32768 ); (* here *)
LDGS tTrunc07.x
CONST -32768
JEQ L2
CONST 0
CONST 12
GLOBAL EASSERT
CALL 2
LABEL L2
! 	y := 50000; x := y;
CONST 50000
STGW tTrunc07.y
LDGW tTrunc07.y
STGS tTrunc07.x
! 	Out.Int(x, 0); Out.Ln;
CONST 0
LDGS tTrunc07.x
GLOBAL Out.Int
CALL 2
GLOBAL Out.Ln
CALL 0
RETURN
END

! Global variables
GLOVAR tTrunc07.x 2
GLOVAR tTrunc07.y 4

! End of file
]]*)
