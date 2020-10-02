MODULE tAck07;

IMPORT Out;

(* Definition:

   Ack(0, n) = n+1
   Ack(m+1, 0) = Ack(m, 1)
   Ack(m+1, n+1) = Ack(m, Ack(m+1, n))

Put Ack(m+1, -1) = 1 so that 

   Ack(m+1, n) = Ack(m, Ack(m+1, n-1))

holds for all values of n. *)

PROCEDURE Ack(M, N: INTEGER): INTEGER;
  VAR 
    m: INTEGER;
    arg, val: ARRAY 10 OF INTEGER;
BEGIN
  arg[0] := 0; val[0] := 1;
  FOR m := 1 TO M DO arg[m] := -1; val[m] := 1 END;

  (* Invariant -- for each i,
	(a) val[i] = Ack(i, arg[i]),  
	(b) arg[i] < val[i],  
	(c) arg[i] < val[i+1] *)

  WHILE arg[M] < N DO
    INC(arg[0]); val[0] := arg[0] + 1;
    m := 0;
    WHILE (m <= M) & (arg[m] = val[m+1]) DO
      (* Ack(m+1, arg[m+1]+1) = Ack(m, Ack(m+1, arg[m+1]))
		= Ack(m, val[m+1]) = Ack(m, arg[m]) = val[m] *)
      INC(arg[m+1]); val[m+1] := val[m]; m := m+1
    END
  END;

  RETURN val[M]
END Ack;

VAR memo: ARRAY 10 OF ARRAY 100000 OF INTEGER;

PROCEDURE Ack2(M, N: INTEGER): INTEGER;
  VAR val: INTEGER;
BEGIN
  IF memo[M, N] # 0 THEN 
    val := memo[M, N]
  ELSE
    IF M = 0 THEN val := N+1
    ELSIF N = 0 THEN val := Ack2(M-1, 1)
    ELSE val := Ack2(M-1, Ack2(M, N-1))
    END;
    memo[M, N] := val
  END
RETURN val
END Ack2;

BEGIN
  Out.Int(Ack(4,1), 0); Out.Ln;
  Out.Int(Ack2(4,1), 0); Out.Ln
END tAck07.

(* In the inner loop, the main invariant is satisfied except that maybe
arg[m] = val[m+1].  In that case, we can reason as follows

      Ack(m+1, arg[m+1]+1) 
        = Ack(m, Ack(m+1, arg[m+1]))
        = Ack(m, val[m+1]) 
        = Ack(m, arg[m]) 
        = val[m],

so the assignments INC(arg[m+1]); val[m+1] := val[m] maintain (a). 
   
Also arg[m+1] < val[m+1] = arg[m] < val[m] so arg[m+1]+1 < val[m],
and the assignments maintain (b).  They also make (c) true for i = m
whilst perhaps making arg[m+1] = val[m+2]. *)

(*<<
65533
65533
>>*)

(*[[
!! (SYMFILE #tAck07 STAMP #tAck07.%main 1 #tAck07.m)
!! (CHKSUM STAMP)
!! 
MODULE tAck07 STAMP 0
IMPORT Out STAMP
ENDHDR

PROC tAck07.Ack 88 4 0
! PROCEDURE Ack(M, N: INTEGER): INTEGER;
!   arg[0] := 0; val[0] := 1;
CONST 0
STLW -44
CONST 1
STLW -84
!   FOR m := 1 TO M DO arg[m] := -1; val[m] := 1 END;
LDLW 12
STLW -88
CONST 1
STLW -4
LABEL L1
LDLW -4
LDLW -88
JGT L2
CONST -1
LOCAL -44
LDLW -4
CONST 10
BOUND 23
STIW
CONST 1
LOCAL -84
LDLW -4
CONST 10
BOUND 23
STIW
INCL -4
JUMP L1
LABEL L2
!   WHILE arg[M] < N DO
LOCAL -44
LDLW 12
CONST 10
BOUND 30
LDIW
LDLW 16
JGEQ L5
!     INC(arg[0]); val[0] := arg[0] + 1;
INCL -44
LDLW -44
INC
STLW -84
!     m := 0;
CONST 0
STLW -4
LABEL L6
!     WHILE (m <= M) & (arg[m] = val[m+1]) DO
LDLW -4
LDLW 12
JGT L2
LOCAL -44
LDLW -4
CONST 10
BOUND 33
LDIW
LOCAL -84
LDLW -4
INC
CONST 10
BOUND 33
LDIW
JNEQ L2
!       INC(arg[m+1]); val[m+1] := val[m]; m := m+1
LOCAL -44
LDLW -4
INC
CONST 10
BOUND 36
INDEXW
DUP 0
LOADW
INC
SWAP
STOREW
LOCAL -84
LDLW -4
CONST 10
BOUND 36
LDIW
LOCAL -84
LDLW -4
INC
CONST 10
BOUND 36
STIW
INCL -4
JUMP L6
LABEL L5
!   RETURN val[M]
LOCAL -84
LDLW 12
CONST 10
BOUND 40
LDIW
RETURN
END

PROC tAck07.Ack2 4 5 0
! PROCEDURE Ack2(M, N: INTEGER): INTEGER;
!   IF memo[M, N] # 0 THEN 
GLOBAL tAck07.memo
LDLW 12
CONST 10
BOUND 48
CONST 100000
TIMES
LDLW 16
CONST 100000
BOUND 48
PLUS
LDIW
JEQZ L17
!     val := memo[M, N]
GLOBAL tAck07.memo
LDLW 12
CONST 10
BOUND 49
CONST 100000
TIMES
LDLW 16
CONST 100000
BOUND 49
PLUS
LDIW
STLW -4
JUMP L10
LABEL L17
!     IF M = 0 THEN val := N+1
LDLW 12
JNEQZ L13
LDLW 16
INC
STLW -4
JUMP L11
LABEL L13
!     ELSIF N = 0 THEN val := Ack2(M-1, 1)
LDLW 16
JNEQZ L15
CONST 1
LDLW 12
DEC
GLOBAL tAck07.Ack2
CALLW 2
STLW -4
JUMP L11
LABEL L15
!     ELSE val := Ack2(M-1, Ack2(M, N-1))
LDLW 16
DEC
LDLW 12
GLOBAL tAck07.Ack2
CALLW 2
LDLW 12
DEC
GLOBAL tAck07.Ack2
CALLW 2
STLW -4
LABEL L11
!     memo[M, N] := val
LDLW -4
GLOBAL tAck07.memo
LDLW 12
CONST 10
BOUND 55
CONST 100000
TIMES
LDLW 16
CONST 100000
BOUND 55
PLUS
STIW
LABEL L10
! RETURN val
LDLW -4
RETURN
END

PROC tAck07.%main 0 4 0
!   Out.Int(Ack(4,1), 0); Out.Ln;
CONST 0
CONST 1
CONST 4
GLOBAL tAck07.Ack
CALLW 2
GLOBAL Out.Int
CALL 2
GLOBAL Out.Ln
CALL 0
!   Out.Int(Ack2(4,1), 0); Out.Ln
CONST 0
CONST 1
CONST 4
GLOBAL tAck07.Ack2
CALLW 2
GLOBAL Out.Int
CALL 2
GLOBAL Out.Ln
CALL 0
RETURN
END

! Global variables
GLOVAR tAck07.memo 4000000

! End of file
]]*)
