MODULE tProcRec07;

IMPORT Out;

TYPE Queue = POINTER TO QueueRec;
  QueueRec = 
   RECORD 
     n: INTEGER;
     Clear: PROCEDURE
   END;

(* Clear -- set the queue to be empty *)
PROCEDURE Clear; 
BEGIN
  Out.String("Clear"); Out.Ln
END Clear;

VAR noqueue: Queue;

BEGIN
  NEW(noqueue);
  noqueue.Clear := Clear;
  noqueue.Clear
END tProcRec07.

(*<<
Clear
>>*)

(*[[
!! (SYMFILE #tProcRec07 STAMP #tProcRec07.%main 1 #tProcRec07.m)
!! (CHKSUM STAMP)
!! 
MODULE tProcRec07 STAMP 0
IMPORT Out STAMP
ENDHDR

PROC tProcRec07.Clear 0 3 0
! PROCEDURE Clear; 
!   Out.String("Clear"); Out.Ln
CONST 6
GLOBAL tProcRec07.%1
GLOBAL Out.String
CALL 2
GLOBAL Out.Ln
CALL 0
RETURN
END

PROC tProcRec07.%main 0 3 0
!   NEW(noqueue);
CONST 8
GLOBAL tProcRec07.QueueRec
GLOBAL NEW
CALLW 2
STGW tProcRec07.noqueue
!   noqueue.Clear := Clear;
GLOBAL tProcRec07.Clear
LDGW tProcRec07.noqueue
NCHECK 22
STNW 4
!   noqueue.Clear
LDGW tProcRec07.noqueue
NCHECK 23
LDNW 4
NCHECK 23
CALL 0
RETURN
END

! Global variables
GLOVAR tProcRec07.noqueue 4

! Global pointer map
DEFINE tProcRec07.%gcmap
WORD GC_POINTER
WORD tProcRec07.noqueue
WORD GC_END

! String "Clear"
DEFINE tProcRec07.%1
STRING 436C65617200

! Descriptor for QueueRec
DEFINE tProcRec07.QueueRec
WORD 0
WORD 0
WORD tProcRec07.QueueRec.%anc

DEFINE tProcRec07.QueueRec.%anc
WORD tProcRec07.QueueRec

! End of file
]]*)
