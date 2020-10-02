MODULE tSystolic;

IMPORT Out, Random;

CONST INF = 859;


(* Tracing output *)

VAR t: INTEGER;

PROCEDURE PrintVal(x: INTEGER);
BEGIN
  IF x = INF THEN Out.String("INF") ELSE Out.Int(x, 0) END
END PrintVal;

PROCEDURE Trace(chid: INTEGER; x: INTEGER);
BEGIN
  Out.String("Time "); Out.Int(t, 0); Out.String(": chan");
  Out.Int(chid, 0); Out.String("."); PrintVal(x); Out.Ln
END Trace;

PROCEDURE Print(x: INTEGER);
BEGIN
  Out.String("Time "); Out.Int(t, 0); Out.String(": print ");
  PrintVal(x); Out.Ln
END Print;

PROCEDURE Max(x, y: INTEGER): INTEGER;
BEGIN
  IF x >= y THEN RETURN x ELSE RETURN y END
END Max;

PROCEDURE Min(x, y: INTEGER): INTEGER;
BEGIN
  IF x <= y THEN RETURN x ELSE RETURN y END
END Min;


(* Processes and channels *)

TYPE
  Status = (Ready, Sleeping, Waking, Dead);

  Process = POINTER TO ProcRec;
  Channel = POINTER TO ChanRec;

  ProcRec = ABSTRACT RECORD 
      pid: INTEGER;
      status: Status; 
      pc, next: INTEGER;
      left, right: Channel;
      buf: INTEGER;
    END;

  ChanRec = RECORD
      chid: INTEGER;
      sender, receiver: Process;
    END;

PROCEDURE (self: Process) Init(pid: INTEGER; left, right: Channel); 
BEGIN
  self.pid := pid;
  self.left := left; self.right := right;
  self.status := Ready;
  self.pc := 0
END Init;

ABSTRACT PROCEDURE (self: Process) Step;

(* Run -- step until the process is sleeping or dead *)
PROCEDURE (self: Process) Run;
BEGIN
  WHILE (self.status = Ready) OR (self.status = Waking) DO
    self.next := self.pc+1;
    self.Step;
    self.pc := self.next
  END
END Run;

(* Get -- hang the process onto its input channel *)
PROCEDURE (self: Process) Get(VAR x: INTEGER);
BEGIN
  CASE self.status OF
      Ready: 
        (* First time -- register with channel and go to sleep *)
        self.left.receiver := self;
        self.status := Sleeping;
        self.next := self.pc (* Try input again when we wake *)
    | Waking:
        (* Second time -- fetch the value and continue *)
        x := self.buf;
        self.status := Ready
  END
END Get;

(* Put -- hang the process onto its output channel *)
PROCEDURE (self: Process) Put(x: INTEGER);
BEGIN
  CASE self.status OF
      Ready:
        self.buf := x;
        self.right.sender := self;
        self.status := Sleeping;
        self.next := self.pc
    | Waking:
        self.status := Ready
  END
END Put;

(* Goto -- jump to a different instruction *)
PROCEDURE (self: Process) Goto(lab: INTEGER);
BEGIN
  self.next := lab
END Goto;

(* Halt -- set status to Dead *)
PROCEDURE (self: Process) Halt;
BEGIN
  self.status := Dead
END Halt;

(* MakeChannel -- create and initialize a channel *)
PROCEDURE MakeChannel(chid: INTEGER): Channel;
  VAR ch: Channel;
BEGIN
  NEW(ch); ch.Init(chid); RETURN ch
END MakeChannel;

PROCEDURE (self: Channel) Init(chid: INTEGER);
BEGIN
  self.chid := chid;
  self.sender := NIL; self.receiver := NIL
END Init;

(* Eval -- check for communication on a channel *)
PROCEDURE (self: Channel) Eval;
  VAR x: INTEGER;
BEGIN
  IF (self.sender # NIL) & (self.receiver # NIL) THEN
    (* Transfer data from sender to receiver and wake both *)
    x := self.sender.buf;
    Trace(self.chid, x);
    self.receiver.buf := x;
    self.sender.status := Waking; self.receiver.status := Waking;
    self.sender := NIL; self.receiver := NIL
  END
END Eval;


(* Specific processes *)

TYPE
  Injector = POINTER TO RECORD (ProcRec) r: INTEGER; x: INTEGER; END;

PROCEDURE MakeInjector(pid: INTEGER; right: Channel): Injector;
  VAR p: Injector;
BEGIN
  NEW(p); p.Init(pid, NIL, right); RETURN p
END MakeInjector;

(* The rules for these 'assembly-level' programs are that any Goto should
   be the last action on a line, and Put or Get should be on a line by 
   themselves, with no side effects in their arguments.  A Goto is just a
   hidden assignment to the next-state variable, and lines with Put or
   Get will be evaluated twice, once when the process makes a Ready -->
   Sleeping transition, and again when it makes a Waking --> Ready 
   transition. The code reads as being very 'self'ish, but that's
   Oberon for you! 

   Since each instruction is executed in a separate invocation of Step,
   there can be no local variables whose values are remembered from one
   line to the next, and all state must be made part of the process
   record. *)

PROCEDURE (self: Injector) Step;
BEGIN
  CASE self.pc OF
      0: self.r := 0
    | 1: IF self.r = N THEN self.Goto(5) END
    | 2: self.x := Random.Roll(100); 
    | 3: self.Put(self.x);
    | 4: INC(self.r); self.Goto(1)
    | 5: self.Put(INF)
    | 6: self.Halt
  END
END Step;


TYPE
  Comparator = POINTER TO RECORD (ProcRec) x, y: INTEGER; END;

PROCEDURE MakeComparator(pid: INTEGER; left, right: Channel): Comparator;
  VAR p: Comparator;
BEGIN
  NEW(p); p.Init(pid, left, right); RETURN p
END MakeComparator;

PROCEDURE (self: Comparator) Step;
BEGIN
  CASE self.pc OF
      0: self.Get(self.x)
    | 1: self.Get(self.y)
    | 2: IF self.y = INF THEN self.Goto(5) END
    | 3: self.Put(Min(self.x, self.y))
    | 4: self.x := Max(self.x, self.y); self.Goto(1)
    | 5: self.Put(self.x)
    | 6: self.Put(INF)
    | 7: self.Halt 
  END
END Step;


TYPE
  Collector = POINTER TO RECORD (ProcRec) x: INTEGER; END;

PROCEDURE MakeCollector(pid: INTEGER; left: Channel): Collector;
  VAR p: Collector;
BEGIN
  NEW(p); p.Init(pid, left, NIL); RETURN p
END MakeCollector;

PROCEDURE (self: Collector) Step;
BEGIN
  CASE self.pc OF
      0: self.Get(self.x)
    | 1: IF self.x = INF THEN self.Goto(3) END
    | 2: Print(self.x); self.Goto(0)
    | 3: self.Halt
  END
END Step;


CONST N = 10;

VAR 
  proc: ARRAY N+2 OF Process;
  chan: ARRAY N+1 OF Channel;

(* Build -- construct processes and channels *)
PROCEDURE Build;
  VAR i: INTEGER;
BEGIN
  FOR i := 0 TO N DO chan[i] := MakeChannel(i) END;

  proc[0] := MakeInjector(0, chan[0]);
  FOR i := 1 TO N DO proc[i] := MakeComparator(i, chan[i-1], chan[i]) END;
  proc[N+1] := MakeCollector(N+1, chan[N]);
END Build;

(* Run -- run the simulation *)
PROCEDURE Run;
  VAR i: INTEGER;
BEGIN
  t := 0;
  WHILE proc[N+1].status # Dead DO
    FOR i := 0 TO N+1 DO proc[i].Run END;
    t := t+1;
    FOR i := 0 TO N DO chan[i].Eval END
  END
END Run;

BEGIN
  Build;
  Run
END tSystolic.

(*<<
Time 1: chan0.16
Time 2: chan0.12
Time 3: chan1.12
Time 4: chan0.94
Time 5: chan1.16
Time 6: chan0.90
Time 6: chan2.12
Time 7: chan1.90
Time 8: chan0.70
Time 8: chan2.16
Time 9: chan1.70
Time 9: chan3.12
Time 10: chan0.83
Time 10: chan2.70
Time 11: chan1.83
Time 11: chan3.16
Time 12: chan0.38
Time 12: chan2.83
Time 12: chan4.12
Time 13: chan1.38
Time 13: chan3.70
Time 14: chan0.7
Time 14: chan2.38
Time 14: chan4.16
Time 15: chan1.7
Time 15: chan3.38
Time 15: chan5.12
Time 16: chan0.63
Time 16: chan2.7
Time 16: chan4.38
Time 17: chan1.63
Time 17: chan3.7
Time 17: chan5.16
Time 18: chan0.15
Time 18: chan2.63
Time 18: chan4.7
Time 18: chan6.12
Time 19: chan1.15
Time 19: chan3.63
Time 19: chan5.7
Time 20: chan0.INF
Time 20: chan2.15
Time 20: chan4.63
Time 20: chan6.7
Time 21: chan1.94
Time 21: chan3.15
Time 21: chan5.38
Time 21: chan7.7
Time 22: chan2.90
Time 22: chan4.15
Time 22: chan6.16
Time 23: chan1.INF
Time 23: chan3.83
Time 23: chan5.15
Time 23: chan7.12
Time 24: chan2.94
Time 24: chan4.70
Time 24: chan6.15
Time 24: chan8.7
Time 25: chan3.90
Time 25: chan5.63
Time 25: chan7.15
Time 26: chan2.INF
Time 26: chan4.83
Time 26: chan6.38
Time 26: chan8.12
Time 27: chan3.94
Time 27: chan5.70
Time 27: chan7.16
Time 27: chan9.7
Time 28: chan4.90
Time 28: chan6.63
Time 28: chan8.15
Time 29: chan3.INF
Time 29: chan5.83
Time 29: chan7.38
Time 29: chan9.12
Time 30: chan4.94
Time 30: chan6.70
Time 30: chan8.16
Time 30: chan10.7
Time 30: print 7
Time 31: chan5.90
Time 31: chan7.63
Time 31: chan9.15
Time 32: chan4.INF
Time 32: chan6.83
Time 32: chan8.38
Time 32: chan10.12
Time 32: print 12
Time 33: chan5.94
Time 33: chan7.70
Time 33: chan9.16
Time 34: chan6.90
Time 34: chan8.63
Time 34: chan10.15
Time 34: print 15
Time 35: chan5.INF
Time 35: chan7.83
Time 35: chan9.38
Time 36: chan6.94
Time 36: chan8.70
Time 36: chan10.16
Time 36: print 16
Time 37: chan7.90
Time 37: chan9.63
Time 38: chan6.INF
Time 38: chan8.83
Time 38: chan10.38
Time 38: print 38
Time 39: chan7.94
Time 39: chan9.70
Time 40: chan8.90
Time 40: chan10.63
Time 40: print 63
Time 41: chan7.INF
Time 41: chan9.83
Time 42: chan8.94
Time 42: chan10.70
Time 42: print 70
Time 43: chan9.90
Time 44: chan8.INF
Time 44: chan10.83
Time 44: print 83
Time 45: chan9.94
Time 46: chan10.90
Time 46: print 90
Time 47: chan9.INF
Time 48: chan10.94
Time 48: print 94
Time 49: chan10.INF
>>*)

(*[[
!! (SYMFILE #tSystolic STAMP #tSystolic.%main 1 #tSystolic.m)
!! (CHKSUM STAMP)
!! 
MODULE tSystolic STAMP 0
IMPORT Out STAMP
IMPORT Random STAMP
ENDHDR

PROC tSystolic.PrintVal 0 3 0
! PROCEDURE PrintVal(x: INTEGER);
!   IF x = INF THEN Out.String("INF") ELSE Out.Int(x, 0) END
LDLW 12
CONST 859
JNEQ L11
CONST 4
GLOBAL tSystolic.%1
GLOBAL Out.String
CALL 2
RETURN
LABEL L11
CONST 0
LDLW 12
GLOBAL Out.Int
CALL 2
RETURN
END

PROC tSystolic.Trace 0 3 0
! PROCEDURE Trace(chid: INTEGER; x: INTEGER);
!   Out.String("Time "); Out.Int(t, 0); Out.String(": chan");
CONST 6
GLOBAL tSystolic.%2
GLOBAL Out.String
CALL 2
CONST 0
LDGW tSystolic.t
GLOBAL Out.Int
CALL 2
CONST 7
GLOBAL tSystolic.%3
GLOBAL Out.String
CALL 2
!   Out.Int(chid, 0); Out.String("."); PrintVal(x); Out.Ln
CONST 0
LDLW 12
GLOBAL Out.Int
CALL 2
CONST 2
GLOBAL tSystolic.%8
GLOBAL Out.String
CALL 2
LDLW 16
GLOBAL tSystolic.PrintVal
CALL 1
GLOBAL Out.Ln
CALL 0
RETURN
END

PROC tSystolic.Print 0 3 0
! PROCEDURE Print(x: INTEGER);
!   Out.String("Time "); Out.Int(t, 0); Out.String(": print ");
CONST 6
GLOBAL tSystolic.%2
GLOBAL Out.String
CALL 2
CONST 0
LDGW tSystolic.t
GLOBAL Out.Int
CALL 2
CONST 9
GLOBAL tSystolic.%4
GLOBAL Out.String
CALL 2
!   PrintVal(x); Out.Ln
LDLW 12
GLOBAL tSystolic.PrintVal
CALL 1
GLOBAL Out.Ln
CALL 0
RETURN
END

PROC tSystolic.Max 0 2 0
! PROCEDURE Max(x, y: INTEGER): INTEGER;
!   IF x >= y THEN RETURN x ELSE RETURN y END
LDLW 12
LDLW 16
JLT L14
LDLW 12
RETURN
LABEL L14
LDLW 16
RETURN
END

PROC tSystolic.Min 0 2 0
! PROCEDURE Min(x, y: INTEGER): INTEGER;
!   IF x <= y THEN RETURN x ELSE RETURN y END
LDLW 12
LDLW 16
JGT L17
LDLW 12
RETURN
LABEL L17
LDLW 16
RETURN
END

PROC tSystolic.ProcRec.Init 0 3 0x00d00001
! PROCEDURE (self: Process) Init(pid: INTEGER; left, right: Channel); 
!   self.pid := pid;
LDLW 16
LDLW 12
NCHECK 63
STOREW
!   self.left := left; self.right := right;
LDLW 20
LDLW 12
NCHECK 64
STNW 16
LDLW 24
LDLW 12
NCHECK 64
STNW 20
!   self.status := Ready;
CONST 0
LDLW 12
NCHECK 65
STNW 4
!   self.pc := 0
CONST 0
LDLW 12
NCHECK 66
STNW 8
RETURN
END

PROC tSystolic.ProcRec.Run 0 3 0x00100001
! PROCEDURE (self: Process) Run;
LABEL L18
!   WHILE (self.status = Ready) OR (self.status = Waking) DO
LDLW 12
NCHECK 74
LDNW 4
JEQZ L19
LDLW 12
NCHECK 74
LDNW 4
CONST 2
JNEQ L20
LABEL L19
!     self.next := self.pc+1;
LDLW 12
NCHECK 75
LDNW 8
INC
LDLW 12
NCHECK 75
STNW 12
!     self.Step;
LDLW 12
NCHECK 76
DUP 0
LDNW -4
LDNW 16
CALL 1
!     self.pc := self.next
LDLW 12
NCHECK 77
LDNW 12
LDLW 12
NCHECK 77
STNW 8
JUMP L18
LABEL L20
RETURN
END

PROC tSystolic.ProcRec.Get 0 3 0x00300001
! PROCEDURE (self: Process) Get(VAR x: INTEGER);
!   CASE self.status OF
LDLW 12
NCHECK 84
LDNW 4
JCASE 3
CASEL L24
CASEL L22
CASEL L25
JUMP L22
LABEL L24
!         self.left.receiver := self;
LDLW 12
LDLW 12
NCHECK 87
LDNW 16
NCHECK 87
STNW 8
!         self.status := Sleeping;
CONST 1
LDLW 12
NCHECK 88
STNW 4
!         self.next := self.pc (* Try input again when we wake *)
LDLW 12
NCHECK 89
LDNW 8
LDLW 12
NCHECK 89
STNW 12
RETURN
LABEL L25
!         x := self.buf;
LDLW 12
NCHECK 92
LDNW 24
LDLW 16
STOREW
!         self.status := Ready
CONST 0
LDLW 12
NCHECK 93
STNW 4
RETURN
LABEL L22
ERROR E_CASE 84
RETURN
END

PROC tSystolic.ProcRec.Put 0 3 0x00100001
! PROCEDURE (self: Process) Put(x: INTEGER);
!   CASE self.status OF
LDLW 12
NCHECK 100
LDNW 4
JCASE 3
CASEL L28
CASEL L26
CASEL L29
JUMP L26
LABEL L28
!         self.buf := x;
LDLW 16
LDLW 12
NCHECK 102
STNW 24
!         self.right.sender := self;
LDLW 12
LDLW 12
NCHECK 103
LDNW 20
NCHECK 103
STNW 4
!         self.status := Sleeping;
CONST 1
LDLW 12
NCHECK 104
STNW 4
!         self.next := self.pc
LDLW 12
NCHECK 105
LDNW 8
LDLW 12
NCHECK 105
STNW 12
RETURN
LABEL L29
!         self.status := Ready
CONST 0
LDLW 12
NCHECK 107
STNW 4
RETURN
LABEL L26
ERROR E_CASE 100
RETURN
END

PROC tSystolic.ProcRec.Goto 0 3 0x00100001
! PROCEDURE (self: Process) Goto(lab: INTEGER);
!   self.next := lab
LDLW 16
LDLW 12
NCHECK 114
STNW 12
RETURN
END

PROC tSystolic.ProcRec.Halt 0 3 0x00100001
! PROCEDURE (self: Process) Halt;
!   self.status := Dead
CONST 3
LDLW 12
NCHECK 120
STNW 4
RETURN
END

PROC tSystolic.MakeChannel 4 4 0x00010001
! PROCEDURE MakeChannel(chid: INTEGER): Channel;
!   NEW(ch); ch.Init(chid); RETURN ch
CONST 12
GLOBAL tSystolic.ChanRec
GLOBAL NEW
CALLW 2
STLW -4
LDLW 12
LDLW -4
NCHECK 127
DUP 0
LDNW -4
LDNW 12
CALL 2
LDLW -4
RETURN
END

PROC tSystolic.ChanRec.Init 0 3 0x00100001
! PROCEDURE (self: Channel) Init(chid: INTEGER);
!   self.chid := chid;
LDLW 16
LDLW 12
NCHECK 132
STOREW
!   self.sender := NIL; self.receiver := NIL
CONST 0
LDLW 12
NCHECK 133
STNW 4
CONST 0
LDLW 12
NCHECK 133
STNW 8
RETURN
END

PROC tSystolic.ChanRec.Eval 4 3 0x00100001
! PROCEDURE (self: Channel) Eval;
!   IF (self.sender # NIL) & (self.receiver # NIL) THEN
LDLW 12
NCHECK 140
LDNW 4
JEQZ L32
LDLW 12
NCHECK 140
LDNW 8
JEQZ L32
!     x := self.sender.buf;
LDLW 12
NCHECK 142
LDNW 4
NCHECK 142
LDNW 24
STLW -4
!     Trace(self.chid, x);
LDLW -4
LDLW 12
NCHECK 143
LOADW
GLOBAL tSystolic.Trace
CALL 2
!     self.receiver.buf := x;
LDLW -4
LDLW 12
NCHECK 144
LDNW 8
NCHECK 144
STNW 24
!     self.sender.status := Waking; self.receiver.status := Waking;
CONST 2
LDLW 12
NCHECK 145
LDNW 4
NCHECK 145
STNW 4
CONST 2
LDLW 12
NCHECK 145
LDNW 8
NCHECK 145
STNW 4
!     self.sender := NIL; self.receiver := NIL
CONST 0
LDLW 12
NCHECK 146
STNW 4
CONST 0
LDLW 12
NCHECK 146
STNW 8
LABEL L32
RETURN
END

PROC tSystolic.MakeInjector 4 6 0x00210001
! PROCEDURE MakeInjector(pid: INTEGER; right: Channel): Injector;
!   NEW(p); p.Init(pid, NIL, right); RETURN p
CONST 36
GLOBAL tSystolic.%5
GLOBAL NEW
CALLW 2
STLW -4
LDLW 16
CONST 0
LDLW 12
LDLW -4
NCHECK 159
DUP 0
LDNW -4
LDNW 12
CALL 4
LDLW -4
RETURN
END

PROC tSystolic.%5.Step 0 4 0x00100001
! PROCEDURE (self: Injector) Step;
!   CASE self.pc OF
LDLW 12
NCHECK 178
LDNW 8
JCASE 7
CASEL L36
CASEL L37
CASEL L38
CASEL L39
CASEL L40
CASEL L41
CASEL L42
JUMP L34
LABEL L36
!       0: self.r := 0
CONST 0
LDLW 12
NCHECK 179
STNW 28
RETURN
LABEL L37
!     | 1: IF self.r = N THEN self.Goto(5) END
LDLW 12
NCHECK 180
LDNW 28
CONST 10
JNEQ L35
CONST 5
LDLW 12
NCHECK 180
DUP 0
LDNW -4
LDNW 32
CALL 2
RETURN
LABEL L38
!     | 2: self.x := Random.Roll(100); 
CONST 100
GLOBAL Random.Roll
CALLW 1
LDLW 12
NCHECK 181
STNW 32
RETURN
LABEL L39
!     | 3: self.Put(self.x);
LDLW 12
NCHECK 182
LDNW 32
LDLW 12
NCHECK 182
DUP 0
LDNW -4
LDNW 28
CALL 2
RETURN
LABEL L40
!     | 4: INC(self.r); self.Goto(1)
LDLW 12
NCHECK 183
DUP 0
LDNW 28
INC
SWAP
STNW 28
CONST 1
LDLW 12
NCHECK 183
DUP 0
LDNW -4
LDNW 32
CALL 2
RETURN
LABEL L41
!     | 5: self.Put(INF)
CONST 859
LDLW 12
NCHECK 184
DUP 0
LDNW -4
LDNW 28
CALL 2
RETURN
LABEL L42
!     | 6: self.Halt
LDLW 12
NCHECK 185
DUP 0
LDNW -4
LDNW 36
CALL 1
RETURN
LABEL L34
ERROR E_CASE 178
LABEL L35
RETURN
END

PROC tSystolic.MakeComparator 4 6 0x00610001
! PROCEDURE MakeComparator(pid: INTEGER; left, right: Channel): Comparator;
!   NEW(p); p.Init(pid, left, right); RETURN p
CONST 36
GLOBAL tSystolic.%6
GLOBAL NEW
CALLW 2
STLW -4
LDLW 20
LDLW 16
LDLW 12
LDLW -4
NCHECK 196
DUP 0
LDNW -4
LDNW 12
CALL 4
LDLW -4
RETURN
END

PROC tSystolic.%6.Step 0 4 0x00100001
! PROCEDURE (self: Comparator) Step;
!   CASE self.pc OF
LDLW 12
NCHECK 201
LDNW 8
JCASE 8
CASEL L48
CASEL L49
CASEL L50
CASEL L51
CASEL L52
CASEL L53
CASEL L54
CASEL L55
JUMP L46
LABEL L48
!       0: self.Get(self.x)
LDLW 12
NCHECK 202
CONST 28
OFFSET
LDLW 12
NCHECK 202
DUP 0
LDNW -4
LDNW 24
CALL 2
RETURN
LABEL L49
!     | 1: self.Get(self.y)
LDLW 12
NCHECK 203
CONST 32
OFFSET
LDLW 12
NCHECK 203
DUP 0
LDNW -4
LDNW 24
CALL 2
RETURN
LABEL L50
!     | 2: IF self.y = INF THEN self.Goto(5) END
LDLW 12
NCHECK 204
LDNW 32
CONST 859
JNEQ L47
CONST 5
LDLW 12
NCHECK 204
DUP 0
LDNW -4
LDNW 32
CALL 2
RETURN
LABEL L51
!     | 3: self.Put(Min(self.x, self.y))
LDLW 12
NCHECK 205
LDNW 32
LDLW 12
NCHECK 205
LDNW 28
GLOBAL tSystolic.Min
CALLW 2
LDLW 12
NCHECK 205
DUP 0
LDNW -4
LDNW 28
CALL 2
RETURN
LABEL L52
!     | 4: self.x := Max(self.x, self.y); self.Goto(1)
LDLW 12
NCHECK 206
LDNW 32
LDLW 12
NCHECK 206
LDNW 28
GLOBAL tSystolic.Max
CALLW 2
LDLW 12
NCHECK 206
STNW 28
CONST 1
LDLW 12
NCHECK 206
DUP 0
LDNW -4
LDNW 32
CALL 2
RETURN
LABEL L53
!     | 5: self.Put(self.x)
LDLW 12
NCHECK 207
LDNW 28
LDLW 12
NCHECK 207
DUP 0
LDNW -4
LDNW 28
CALL 2
RETURN
LABEL L54
!     | 6: self.Put(INF)
CONST 859
LDLW 12
NCHECK 208
DUP 0
LDNW -4
LDNW 28
CALL 2
RETURN
LABEL L55
!     | 7: self.Halt 
LDLW 12
NCHECK 209
DUP 0
LDNW -4
LDNW 36
CALL 1
RETURN
LABEL L46
ERROR E_CASE 201
LABEL L47
RETURN
END

PROC tSystolic.MakeCollector 4 6 0x00210001
! PROCEDURE MakeCollector(pid: INTEGER; left: Channel): Collector;
!   NEW(p); p.Init(pid, left, NIL); RETURN p
CONST 32
GLOBAL tSystolic.%7
GLOBAL NEW
CALLW 2
STLW -4
CONST 0
LDLW 16
LDLW 12
LDLW -4
NCHECK 220
DUP 0
LDNW -4
LDNW 12
CALL 4
LDLW -4
RETURN
END

PROC tSystolic.%7.Step 0 4 0x00100001
! PROCEDURE (self: Collector) Step;
!   CASE self.pc OF
LDLW 12
NCHECK 225
LDNW 8
JCASE 4
CASEL L61
CASEL L62
CASEL L63
CASEL L64
JUMP L59
LABEL L61
!       0: self.Get(self.x)
LDLW 12
NCHECK 226
CONST 28
OFFSET
LDLW 12
NCHECK 226
DUP 0
LDNW -4
LDNW 24
CALL 2
RETURN
LABEL L62
!     | 1: IF self.x = INF THEN self.Goto(3) END
LDLW 12
NCHECK 227
LDNW 28
CONST 859
JNEQ L60
CONST 3
LDLW 12
NCHECK 227
DUP 0
LDNW -4
LDNW 32
CALL 2
RETURN
LABEL L63
!     | 2: Print(self.x); self.Goto(0)
LDLW 12
NCHECK 228
LDNW 28
GLOBAL tSystolic.Print
CALL 1
CONST 0
LDLW 12
NCHECK 228
DUP 0
LDNW -4
LDNW 32
CALL 2
RETURN
LABEL L64
!     | 3: self.Halt
LDLW 12
NCHECK 229
DUP 0
LDNW -4
LDNW 36
CALL 1
RETURN
LABEL L59
ERROR E_CASE 225
LABEL L60
RETURN
END

PROC tSystolic.Build 4 4 0
! PROCEDURE Build;
!   FOR i := 0 TO N DO chan[i] := MakeChannel(i) END;
CONST 0
STLW -4
LABEL L68
LDLW -4
CONST 10
JGT L69
LDLW -4
GLOBAL tSystolic.MakeChannel
CALLW 1
GLOBAL tSystolic.chan
LDLW -4
CONST 11
BOUND 244
STIW
INCL -4
JUMP L68
LABEL L69
!   proc[0] := MakeInjector(0, chan[0]);
LDGW tSystolic.chan
CONST 0
GLOBAL tSystolic.MakeInjector
CALLW 2
STGW tSystolic.proc
!   FOR i := 1 TO N DO proc[i] := MakeComparator(i, chan[i-1], chan[i]) END;
CONST 1
STLW -4
LABEL L70
LDLW -4
CONST 10
JGT L71
GLOBAL tSystolic.chan
LDLW -4
CONST 11
BOUND 247
LDIW
GLOBAL tSystolic.chan
LDLW -4
DEC
CONST 11
BOUND 247
LDIW
LDLW -4
GLOBAL tSystolic.MakeComparator
CALLW 3
GLOBAL tSystolic.proc
LDLW -4
CONST 12
BOUND 247
STIW
INCL -4
JUMP L70
LABEL L71
!   proc[N+1] := MakeCollector(N+1, chan[N]);
GLOBAL tSystolic.chan
LDNW 40
CONST 11
GLOBAL tSystolic.MakeCollector
CALLW 2
GLOBAL tSystolic.proc
STNW 44
RETURN
END

PROC tSystolic.Run 4 3 0
! PROCEDURE Run;
!   t := 0;
CONST 0
STGW tSystolic.t
LABEL L72
!   WHILE proc[N+1].status # Dead DO
GLOBAL tSystolic.proc
LDNW 44
NCHECK 256
LDNW 4
CONST 3
JEQ L74
!     FOR i := 0 TO N+1 DO proc[i].Run END;
CONST 0
STLW -4
LABEL L75
LDLW -4
CONST 11
JGT L76
GLOBAL tSystolic.proc
LDLW -4
CONST 12
BOUND 257
LDIW
NCHECK 257
DUP 0
LDNW -4
LDNW 20
CALL 1
INCL -4
JUMP L75
LABEL L76
!     t := t+1;
LDGW tSystolic.t
INC
STGW tSystolic.t
!     FOR i := 0 TO N DO chan[i].Eval END
CONST 0
STLW -4
LABEL L77
LDLW -4
CONST 10
JGT L72
GLOBAL tSystolic.chan
LDLW -4
CONST 11
BOUND 259
LDIW
NCHECK 259
DUP 0
LDNW -4
LDNW 16
CALL 1
INCL -4
JUMP L77
LABEL L74
RETURN
END

PROC tSystolic.%main 0 1 0
!   Build;
GLOBAL tSystolic.Build
CALL 0
!   Run
GLOBAL tSystolic.Run
CALL 0
RETURN
END

! Global variables
GLOVAR tSystolic.t 4
GLOVAR tSystolic.proc 48
GLOVAR tSystolic.chan 44

! Global pointer map
DEFINE tSystolic.%gcmap
WORD GC_BASE
WORD tSystolic.proc
WORD 0x00001fff
WORD GC_BASE
WORD tSystolic.chan
WORD 0x00000fff
WORD GC_END

! String "INF"
DEFINE tSystolic.%1
STRING 494E4600

! String "Time "
DEFINE tSystolic.%2
STRING 54696D652000

! String ": chan"
DEFINE tSystolic.%3
STRING 3A206368616E00

! String ": print "
DEFINE tSystolic.%4
STRING 3A207072696E742000

! String "."
DEFINE tSystolic.%8
STRING 2E00

! Descriptor for ProcRec
DEFINE tSystolic.ProcRec
WORD 0

! Descriptor for ChanRec
DEFINE tSystolic.ChanRec
WORD 0x0000000d
WORD 0
WORD tSystolic.ChanRec.%anc
WORD tSystolic.ChanRec.Init
WORD tSystolic.ChanRec.Eval

DEFINE tSystolic.ChanRec.%anc
WORD tSystolic.ChanRec

! Descriptor for *anon*
DEFINE tSystolic.%5
WORD 0x00000061
WORD 1
WORD tSystolic.%5.%anc
WORD tSystolic.ProcRec.Init
WORD tSystolic.%5.Step
WORD tSystolic.ProcRec.Run
WORD tSystolic.ProcRec.Get
WORD tSystolic.ProcRec.Put
WORD tSystolic.ProcRec.Goto
WORD tSystolic.ProcRec.Halt

DEFINE tSystolic.%5.%anc
WORD tSystolic.ProcRec
WORD tSystolic.%5

! Descriptor for *anon*
DEFINE tSystolic.%6
WORD 0x00000061
WORD 1
WORD tSystolic.%6.%anc
WORD tSystolic.ProcRec.Init
WORD tSystolic.%6.Step
WORD tSystolic.ProcRec.Run
WORD tSystolic.ProcRec.Get
WORD tSystolic.ProcRec.Put
WORD tSystolic.ProcRec.Goto
WORD tSystolic.ProcRec.Halt

DEFINE tSystolic.%6.%anc
WORD tSystolic.ProcRec
WORD tSystolic.%6

! Descriptor for *anon*
DEFINE tSystolic.%7
WORD 0x00000061
WORD 1
WORD tSystolic.%7.%anc
WORD tSystolic.ProcRec.Init
WORD tSystolic.%7.Step
WORD tSystolic.ProcRec.Run
WORD tSystolic.ProcRec.Get
WORD tSystolic.ProcRec.Put
WORD tSystolic.ProcRec.Goto
WORD tSystolic.ProcRec.Halt

DEFINE tSystolic.%7.%anc
WORD tSystolic.ProcRec
WORD tSystolic.%7

! End of file
]]*)
