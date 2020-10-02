MODULE tGCBug07;

(* Provoke GC bug in release_phase (r1556). *)

(*<<
OK
>>*)

IMPORT SYSTEM, Out;

TYPE blob = POINTER TO ARRAY 2044 OF CHAR;
  flab = POINTER TO ARRAY 4092 OF CHAR;

VAR p, q, t: blob; u: flab;

BEGIN
  (* Allocate one page of blobs *)
  NEW(p); NEW(q);
  (* Allocate a full page object *)
  NEW(u);
  (* One more blob not adjacent to the others *)
  NEW(t);

  t := NIL;
  SYSTEM.GC;

  (* OK if we haven't segfaulted yet *)
  Out.String("OK"); Out.Ln
END tGCBug07.

(*[[
!! (SYMFILE #tGCBug07 STAMP #tGCBug07.%main 1 #tGCBug07.m)
!! (CHKSUM STAMP)
!! 
MODULE tGCBug07 STAMP 0
IMPORT Out STAMP
ENDHDR

PROC tGCBug07.%main 0 3 0
!   NEW(p); NEW(q);
CONST 2044
CONST 0
GLOBAL NEW
CALLW 2
STGW tGCBug07.p
CONST 2044
CONST 0
GLOBAL NEW
CALLW 2
STGW tGCBug07.q
!   NEW(u);
CONST 4092
CONST 0
GLOBAL NEW
CALLW 2
STGW tGCBug07.u
!   NEW(t);
CONST 2044
CONST 0
GLOBAL NEW
CALLW 2
STGW tGCBug07.t
!   t := NIL;
CONST 0
STGW tGCBug07.t
!   SYSTEM.GC;
GLOBAL SYSTEM.GC
CALL 0
!   Out.String("OK"); Out.Ln
CONST 3
GLOBAL tGCBug07.%1
GLOBAL Out.String
CALL 2
GLOBAL Out.Ln
CALL 0
RETURN
END

! Global variables
GLOVAR tGCBug07.p 4
GLOVAR tGCBug07.q 4
GLOVAR tGCBug07.t 4
GLOVAR tGCBug07.u 4

! Global pointer map
DEFINE tGCBug07.%gcmap
WORD GC_POINTER
WORD tGCBug07.p
WORD GC_POINTER
WORD tGCBug07.q
WORD GC_POINTER
WORD tGCBug07.t
WORD GC_POINTER
WORD tGCBug07.u
WORD GC_END

! String "OK"
DEFINE tGCBug07.%1
STRING 4F4B00

! End of file
]]*)
