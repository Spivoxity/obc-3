MODULE tBitmap;

IMPORT Out;

(* Bug: pointer maps passed to NEWFLEX as hexadecimal constants confuse
the stack simulator *)

VAR a : POINTER TO ARRAY OF ARRAY 2 OF POINTER TO ARRAY OF INTEGER;
  i, j : INTEGER;
    
BEGIN
  i := 4; j := 4;
  NEW(a, 5); NEW(a[4][1], 10);
  a[i][1][2*j] := 42;
  Out.Int(a[j][1][2*i], 0); Out.Ln
END tBitmap.

(*<<
42
>>*)

(*[[
!! (SYMFILE #tBitmap STAMP #tBitmap.%main 1 #tBitmap.m)
!! (CHKSUM STAMP)
!! 
MODULE tBitmap STAMP 0
IMPORT Out STAMP
ENDHDR

PROC tBitmap.%main 0 5 0
!   i := 4; j := 4;
CONST 4
STGW tBitmap.i
CONST 4
STGW tBitmap.j
!   NEW(a, 5); NEW(a[4][1], 10);
CONST 5
CONST 1
CONST 8
CONST 0x00000007
GLOBAL NEWFLEX
CALLW 4
STGW tBitmap.a
CONST 10
CONST 1
CONST 4
CONST 0
GLOBAL NEWFLEX
CALLW 4
LDGW tBitmap.a
NCHECK 13
CONST 4
DUP 1
LDNW -4
LDNW 4
BOUND 13
CONST 2
TIMES
INC
STIW
!   a[i][1][2*j] := 42;
CONST 42
LDGW tBitmap.a
NCHECK 14
LDGW tBitmap.i
DUP 1
LDNW -4
LDNW 4
BOUND 14
CONST 2
TIMES
INC
LDIW
NCHECK 14
LDGW tBitmap.j
CONST 2
TIMES
DUP 1
LDNW -4
LDNW 4
BOUND 14
STIW
!   Out.Int(a[j][1][2*i], 0); Out.Ln
CONST 0
LDGW tBitmap.a
NCHECK 15
LDGW tBitmap.j
DUP 1
LDNW -4
LDNW 4
BOUND 15
CONST 2
TIMES
INC
LDIW
NCHECK 15
LDGW tBitmap.i
CONST 2
TIMES
DUP 1
LDNW -4
LDNW 4
BOUND 15
LDIW
GLOBAL Out.Int
CALL 2
GLOBAL Out.Ln
CALL 0
RETURN
END

! Global variables
GLOVAR tBitmap.a 4
GLOVAR tBitmap.i 4
GLOVAR tBitmap.j 4

! Global pointer map
DEFINE tBitmap.%gcmap
WORD GC_POINTER
WORD tBitmap.a
WORD GC_END

! End of file
]]*)
