MODULE tEcho;

(*<<
Runtime error: string copy overflows destination in module Args
In procedure Args.GetArg
   called from tEcho.Echo
   called from tEcho.%main
   called from MAIN
dummy arguments
dummy arguments
>>*)

IMPORT Args, Out;

PROCEDURE Echo(n: INTEGER);
  VAR i: INTEGER; s: POINTER TO ARRAY OF CHAR;
BEGIN
  NEW(s, n);
  FOR i := 1 TO Args.argc-1 DO
    Args.GetArg(i, s^);
    IF i > 1 THEN Out.Char(' ') END;
    Out.String(s^)
  END;
  Out.Ln
END Echo;

BEGIN
  Echo(20);
  Echo(10);
  Echo(5)
END tEcho.

(*[[
!! (SYMFILE #tEcho STAMP #tEcho.%main 1)
!! (CHKSUM STAMP)
!! 
MODULE tEcho STAMP 0
IMPORT Args STAMP
IMPORT Out STAMP
ENDHDR

PROC tEcho.Echo 12 5 0x00008001
! PROCEDURE Echo(n: INTEGER);
!   NEW(s, n);
LDLW 12
CONST 1
CONST 1
CONST 0
GLOBAL NEWFLEX
CALLW 4
STLW -8
!   FOR i := 1 TO Args.argc-1 DO
LDGW Args.argc
DEC
STLW -12
CONST 1
STLW -4
LABEL L1
LDLW -4
LDLW -12
JGT L2
!     Args.GetArg(i, s^);
LDLW -8
NCHECK 20
DUP 0
LDNW -4
LDNW 4
SWAP
LDLW -4
GLOBAL Args.GetArg
CALL 3
!     IF i > 1 THEN Out.Char(' ') END;
LDLW -4
CONST 1
JLE L5
CONST 32
ALIGNC
GLOBAL Out.Char
CALL 1
LABEL L5
!     Out.String(s^)
LDLW -8
NCHECK 22
DUP 0
LDNW -4
LDNW 4
SWAP
GLOBAL Out.String
CALL 2
!   FOR i := 1 TO Args.argc-1 DO
INCL -4
JUMP L1
LABEL L2
!   Out.Ln
GLOBAL Out.Ln
CALL 0
RETURN
END

PROC tEcho.%main 0 5 0
!   Echo(20);
CONST 20
GLOBAL tEcho.Echo
CALL 1
!   Echo(10);
CONST 10
GLOBAL tEcho.Echo
CALL 1
!   Echo(5)
CONST 5
GLOBAL tEcho.Echo
CALL 1
RETURN
END

! End of file
]]*)
