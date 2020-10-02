MODULE tGCBug;

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
END tGCBug.

(*[[
!! (SYMFILE #tGCBug STAMP #tGCBug.%main 1 #tGCBug.m)
!! (CHKSUM STAMP)
!! 
MODULE tGCBug STAMP 0
IMPORT Out STAMP
ENDHDR

PROC tGCBug.%main 0 3 0
!   NEW(p); NEW(q);
CONST 2044
CONST 0
GLOBAL NEW
CALLW 2
STGW tGCBug.p
CONST 2044
CONST 0
GLOBAL NEW
CALLW 2
STGW tGCBug.q
!   NEW(u);
CONST 4092
CONST 0
GLOBAL NEW
CALLW 2
STGW tGCBug.u
!   NEW(t);
CONST 2044
CONST 0
GLOBAL NEW
CALLW 2
STGW tGCBug.t
!   t := NIL;
CONST 0
STGW tGCBug.t
!   SYSTEM.GC;
GLOBAL SYSTEM.GC
CALL 0
!   Out.String("OK"); Out.Ln
CONST 3
GLOBAL tGCBug.%1
GLOBAL Out.String
CALL 2
GLOBAL Out.Ln
CALL 0
RETURN
END

! Global variables
GLOVAR tGCBug.p 4
GLOVAR tGCBug.q 4
GLOVAR tGCBug.t 4
GLOVAR tGCBug.u 4

! Global pointer map
DEFINE tGCBug.%gcmap
WORD GC_POINTER
WORD tGCBug.p
WORD GC_POINTER
WORD tGCBug.q
WORD GC_POINTER
WORD tGCBug.t
WORD GC_POINTER
WORD tGCBug.u
WORD GC_END

! String "OK"
DEFINE tGCBug.%1
STRING 4F4B00

! End of file
]]*)
