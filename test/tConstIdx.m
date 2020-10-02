MODULE tConstIdx;

(* JIT should fold shifts of constants *)

IMPORT Out;

VAR a: POINTER TO ARRAY OF INTEGER;

BEGIN
  NEW(a, 10);
  a[0] := 43;
  a[1] := 37;
  Out.Int(a[0]+a[1], 0); Out.Ln
END tConstIdx.

(*<<
80
>>*)

(*[[
!! (SYMFILE #tConstIdx STAMP #tConstIdx.%main 1 #tConstIdx.m)
!! (CHKSUM STAMP)
!! 
MODULE tConstIdx STAMP 0
IMPORT Out STAMP
ENDHDR

PROC tConstIdx.%main 0 6 0
!   NEW(a, 10);
CONST 10
CONST 1
CONST 4
CONST 0
GLOBAL NEWFLEX
CALLW 4
STGW tConstIdx.a
!   a[0] := 43;
CONST 43
LDGW tConstIdx.a
NCHECK 11
CONST 0
DUP 1
LDNW -4
LDNW 4
BOUND 11
STIW
!   a[1] := 37;
CONST 37
LDGW tConstIdx.a
NCHECK 12
CONST 1
DUP 1
LDNW -4
LDNW 4
BOUND 12
STIW
!   Out.Int(a[0]+a[1], 0); Out.Ln
CONST 0
LDGW tConstIdx.a
NCHECK 13
CONST 0
DUP 1
LDNW -4
LDNW 4
BOUND 13
LDIW
LDGW tConstIdx.a
NCHECK 13
CONST 1
DUP 1
LDNW -4
LDNW 4
BOUND 13
LDIW
PLUS
GLOBAL Out.Int
CALL 2
GLOBAL Out.Ln
CALL 0
RETURN
END

! Global variables
GLOVAR tConstIdx.a 4

! Global pointer map
DEFINE tConstIdx.%gcmap
WORD GC_POINTER
WORD tConstIdx.a
WORD GC_END

! End of file
]]*)
