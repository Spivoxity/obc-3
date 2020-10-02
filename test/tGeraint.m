MODULE tGeraint;

(* Issue #15 *)

IMPORT Out;

TYPE p0 = POINTER TO r0;
  r0 = RECORD END;

PROCEDURE (self: p0) m(q: PROCEDURE (x, w: INTEGER)); END m;

TYPE p1 = POINTER TO r1;
  r1 = RECORD (r0) END;

PROCEDURE (self: p1) m(q: PROCEDURE (x, w: INTEGER));
BEGIN
  q(42, 0); Out.Ln
END m;

VAR v0: p0; v1: p1;

BEGIN
  NEW(v1); v0 := v1; v0.m(Out.Int)
END tGeraint.

(*<<
42
>>*)

(*[[
!! (SYMFILE #tGeraint STAMP #tGeraint.%main 1 #tGeraint.m)
!! (CHKSUM STAMP)
!! 
MODULE tGeraint STAMP 0
IMPORT Out STAMP
ENDHDR

PROC tGeraint.r0.m 0 0 0x00100001
! PROCEDURE (self: p0) m(q: PROCEDURE (x, w: INTEGER)); END m;
RETURN
END

PROC tGeraint.r1.m 0 3 0x00100001
! PROCEDURE (self: p1) m(q: PROCEDURE (x, w: INTEGER));
!   q(42, 0); Out.Ln
CONST 0
CONST 42
LDLW 20
STATLINK
LDLW 16
NCHECK 17
CALL 2
GLOBAL Out.Ln
CALL 0
RETURN
END

PROC tGeraint.%main 0 5 0
!   NEW(v1); v0 := v1; v0.m(Out.Int)
CONST 0
GLOBAL tGeraint.r1
GLOBAL NEW
CALLW 2
STGW tGeraint.v1
LDGW tGeraint.v1
STGW tGeraint.v0
CONST 0
GLOBAL Out.Int
LDGW tGeraint.v0
NCHECK 23
DUP 0
LDNW -4
LDNW 12
CALL 3
RETURN
END

! Global variables
GLOVAR tGeraint.v0 4
GLOVAR tGeraint.v1 4

! Global pointer map
DEFINE tGeraint.%gcmap
WORD GC_POINTER
WORD tGeraint.v0
WORD GC_POINTER
WORD tGeraint.v1
WORD GC_END

! Descriptor for r0
DEFINE tGeraint.r0
WORD 0
WORD 0
WORD tGeraint.r0.%anc
WORD tGeraint.r0.m

DEFINE tGeraint.r0.%anc
WORD tGeraint.r0

! Descriptor for r1
DEFINE tGeraint.r1
WORD 0
WORD 1
WORD tGeraint.r1.%anc
WORD tGeraint.r1.m

DEFINE tGeraint.r1.%anc
WORD tGeraint.r0
WORD tGeraint.r1

! End of file
]]*)
