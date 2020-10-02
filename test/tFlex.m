MODULE tFlex;

(*<<
1
1
>>*)

IMPORT Out;

PROCEDURE PostIncr(VAR x: INTEGER): INTEGER;
  VAR y: INTEGER;
BEGIN
  y := x;
  INC(x);
  RETURN y
END PostIncr;

PROCEDURE Dummy(a: ARRAY OF INTEGER); BEGIN a[0] := a[0] END Dummy;

VAR 
  i: INTEGER;
  xxx: ARRAY 2 OF POINTER TO ARRAY OF INTEGER;

BEGIN
  NEW(xxx[0], 5);
  NEW(xxx[1], 5);
  i := 0;
  xxx[PostIncr(i)][0] := 7;
  Out.Int(i,0); Out.Ln;

  i := 0;
  Dummy(xxx[PostIncr(i)]^);
  Out.Int(i,0); Out.Ln
END tFlex.

(*[[
!! (SYMFILE #tFlex STAMP #tFlex.%main 1 #tFlex.m)
!! (CHKSUM STAMP)
!! 
MODULE tFlex STAMP 0
IMPORT Out STAMP
ENDHDR

PROC tFlex.PostIncr 4 3 0x00100001
! PROCEDURE PostIncr(VAR x: INTEGER): INTEGER;
!   y := x;
LDLW 12
LOADW
STLW -4
!   INC(x);
LDLW 12
DUP 0
LOADW
INC
SWAP
STOREW
!   RETURN y
LDLW -4
RETURN
END

PROC tFlex.Dummy 0 4 0
! PROCEDURE Dummy(a: ARRAY OF INTEGER); BEGIN a[0] := a[0] END Dummy;
LOCAL 12
LDLW 16
CONST 4
TIMES
FLEXCOPY
LDLW 12
CONST 0
LDLW 16
BOUND 18
LDIW
LDLW 12
CONST 0
LDLW 16
BOUND 18
STIW
RETURN
END

PROC tFlex.%main 0 5 0
!   NEW(xxx[0], 5);
CONST 5
CONST 1
CONST 4
CONST 0
GLOBAL NEWFLEX
CALLW 4
STGW tFlex.xxx
!   NEW(xxx[1], 5);
CONST 5
CONST 1
CONST 4
CONST 0
GLOBAL NEWFLEX
CALLW 4
GLOBAL tFlex.xxx
STNW 4
!   i := 0;
CONST 0
STGW tFlex.i
!   xxx[PostIncr(i)][0] := 7;
CONST 7
GLOBAL tFlex.xxx
GLOBAL tFlex.i
GLOBAL tFlex.PostIncr
CALLW 1
CONST 2
BOUND 28
LDIW
NCHECK 28
CONST 0
DUP 1
LDNW -4
LDNW 4
BOUND 28
STIW
!   Out.Int(i,0); Out.Ln;
CONST 0
LDGW tFlex.i
GLOBAL Out.Int
CALL 2
GLOBAL Out.Ln
CALL 0
!   i := 0;
CONST 0
STGW tFlex.i
!   Dummy(xxx[PostIncr(i)]^);
GLOBAL tFlex.xxx
GLOBAL tFlex.i
GLOBAL tFlex.PostIncr
CALLW 1
CONST 2
BOUND 32
LDIW
NCHECK 32
DUP 0
LDNW -4
LDNW 4
SWAP
GLOBAL tFlex.Dummy
CALL 2
!   Out.Int(i,0); Out.Ln
CONST 0
LDGW tFlex.i
GLOBAL Out.Int
CALL 2
GLOBAL Out.Ln
CALL 0
RETURN
END

! Global variables
GLOVAR tFlex.i 4
GLOVAR tFlex.xxx 8

! Global pointer map
DEFINE tFlex.%gcmap
WORD GC_BASE
WORD tFlex.xxx
WORD 0x00000007
WORD GC_END

! End of file
]]*)
