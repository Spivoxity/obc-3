MODULE tGCBug;

(* Provoke GC bug in release_phase (r1556). *)

(*<<
OK
>>*)

IMPORT GC, Out;

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
  GC.Collect;

  (* OK if we haven't segfaulted yet *)
  Out.String("OK"); Out.Ln
END tGCBug.

(*[[
!! SYMFILE #tGCBug STAMP #tGCBug.%main 1
!! END STAMP
!! 
MODULE tGCBug STAMP 0
IMPORT GC STAMP
IMPORT Out STAMP
ENDHDR

PROC tGCBug.%main 0 16 0
!   NEW(p); NEW(q);
CONST 2044
CONST 0
CONST tGCBug.p
CONST NEW
CALL 3
CONST 2044
CONST 0
CONST tGCBug.q
CONST NEW
CALL 3
!   NEW(u);
CONST 4092
CONST 0
CONST tGCBug.u
CONST NEW
CALL 3
!   NEW(t);
CONST 2044
CONST 0
CONST tGCBug.t
CONST NEW
CALL 3
!   t := NIL;
CONST 0
STGW tGCBug.t
!   GC.Collect;
CONST GC.Collect
CALL 0
!   Out.String("OK"); Out.Ln
CONST 3
CONST tGCBug.%1
CONST Out.String
CALL 2
CONST Out.Ln
CALL 0
RETURN
END

! Global variables
GLOBAL tGCBug.p 4
GLOBAL tGCBug.q 4
GLOBAL tGCBug.t 4
GLOBAL tGCBug.u 4

! Pointer map
DEFINE tGCBug.%gcmap
WORD GC_BASE
WORD tGCBug.p
WORD 0
WORD GC_BASE
WORD tGCBug.q
WORD 0
WORD GC_BASE
WORD tGCBug.t
WORD 0
WORD GC_BASE
WORD tGCBug.u
WORD 0
WORD GC_END

! String "OK"
DEFINE tGCBug.%1
STRING 4F4B00

! End of file
]]*)
