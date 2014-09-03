MODULE tSystolic07;

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
  VAR r: INTEGER;
BEGIN
  IF x >= y THEN r := x ELSE r := y END
RETURN r
END Max;

PROCEDURE Min(x, y: INTEGER): INTEGER;
  VAR r: INTEGER;
BEGIN
  IF x <= y THEN r := x ELSE r := y END
RETURN r
END Min;


(* Processes and channels *)

CONST
  Ready = 1; Sleeping = 2; Waking = 3; Dead = 0;

TYPE
  Process = POINTER TO ProcRec;
  Channel = POINTER TO ChanRec;

  ProcRec = RECORD 
      pid: INTEGER;
      status: INTEGER; 
      pc, next: INTEGER;
      left, right: Channel;
      buf: INTEGER;
      step: PROCEDURE (self: Process);
    END;

  ChanRec = RECORD
      chid: INTEGER;
      sender, receiver: Process;
    END;

PROCEDURE Init(self: Process; pid: INTEGER; left, right: Channel; 
                     step: PROCEDURE (self: Process)); 
BEGIN
  self.pid := pid;
  self.left := left; self.right := right;
  self.step := step;
  self.status := Ready;
  self.pc := 0
END Init;

(* Run -- step until the process is sleeping or dead *)
PROCEDURE Run(self: Process);
BEGIN
  WHILE (self.status = Ready) OR (self.status = Waking) DO
    self.next := self.pc+1;
    self.step(self);
    self.pc := self.next
  END
END Run;

(* Get -- hang the process onto its input channel *)
PROCEDURE Get(self: Process; VAR x: INTEGER);
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
PROCEDURE Put(self: Process; x: INTEGER);
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
PROCEDURE Goto(self: Process; lab: INTEGER);
BEGIN
  self.next := lab
END Goto;

(* Halt -- set status to Dead *)
PROCEDURE Halt(self: Process);
BEGIN
  self.status := Dead
END Halt;

(* MakeChannel -- create and initialize a channel *)
PROCEDURE MakeChannel(chid: INTEGER): Channel;
  VAR ch: Channel;
BEGIN
  NEW(ch); InitChan(ch, chid); RETURN ch
END MakeChannel;

PROCEDURE InitChan(self: Channel; chid: INTEGER);
BEGIN
  self.chid := chid;
  self.sender := NIL; self.receiver := NIL
END InitChan;

(* Eval -- check for communication on a channel *)
PROCEDURE Eval(self: Channel);
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
  NEW(p); Init(p, pid, NIL, right, StepInj); RETURN p
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

PROCEDURE StepInj(self0: Process);
  VAR self: Injector;
BEGIN
  self := self0(Injector);
  CASE self.pc OF
      0: self.r := 0
    | 1: IF self.r = N THEN Goto(self, 5) END
    | 2: self.x := Random.Roll(100); 
    | 3: Put(self, self.x);
    | 4: INC(self.r); Goto(self, 1)
    | 5: Put(self, INF)
    | 6: Halt(self)
  END
END StepInj;


TYPE
  Comparator = POINTER TO RECORD (ProcRec) x, y: INTEGER; END;

PROCEDURE MakeComparator(pid: INTEGER; left, right: Channel): Comparator;
  VAR p: Comparator;
BEGIN
  NEW(p); Init(p, pid, left, right, StepComp); RETURN p
END MakeComparator;

PROCEDURE StepComp(self0: Process);
  VAR self: Comparator;
BEGIN
  self := self0(Comparator);
  CASE self.pc OF
      0: Get(self, self.x)
    | 1: Get(self, self.y)
    | 2: IF self.y = INF THEN Goto(self, 5) END
    | 3: Put(self, Min(self.x, self.y))
    | 4: self.x := Max(self.x, self.y); Goto(self, 1)
    | 5: Put(self, self.x)
    | 6: Put(self, INF)
    | 7: Halt(self)
  END
END StepComp;


TYPE
  Collector = POINTER TO RECORD (ProcRec) x: INTEGER; END;

PROCEDURE MakeCollector(pid: INTEGER; left: Channel): Collector;
  VAR p: Collector;
BEGIN
  NEW(p); Init(p, pid, left, NIL, StepColl); RETURN p
END MakeCollector;

PROCEDURE StepColl(self0: Process);
  VAR self: Collector;
BEGIN
  self := self0(Collector);
  CASE self.pc OF
      0: Get(self, self.x)
    | 1: IF self.x = INF THEN Goto(self, 3) END
    | 2: Print(self.x); Goto(self, 0)
    | 3: Halt(self)
  END
END StepColl;


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

(* RunSim -- run the simulation *)
PROCEDURE RunSim;
  VAR i: INTEGER;
BEGIN
  t := 0;
  WHILE proc[N+1].status # Dead DO
    FOR i := 0 TO N+1 DO Run(proc[i]) END;
    t := t+1;
    FOR i := 0 TO N DO Eval(chan[i]) END
  END
END RunSim;

BEGIN
  Build;
  RunSim
END tSystolic07.

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
!! SYMFILE #tSystolic07 STAMP #tSystolic07.%main 1
!! END STAMP
!! 
MODULE tSystolic07 STAMP 0
IMPORT Out STAMP
IMPORT Random STAMP
ENDHDR

PROC tSystolic07.PrintVal 0 3 0
! PROCEDURE PrintVal(x: INTEGER);
!   IF x = INF THEN Out.String("INF") ELSE Out.Int(x, 0) END
LDLW 12
CONST 859
JNEQ 11
CONST 4
GLOBAL tSystolic07.%1
GLOBAL Out.String
CALL 2
RETURN
LABEL 11
CONST 0
LDLW 12
GLOBAL Out.Int
CALL 2
RETURN
END

PROC tSystolic07.Trace 0 3 0
! PROCEDURE Trace(chid: INTEGER; x: INTEGER);
!   Out.String("Time "); Out.Int(t, 0); Out.String(": chan");
CONST 6
GLOBAL tSystolic07.%2
GLOBAL Out.String
CALL 2
CONST 0
LDGW tSystolic07.t
GLOBAL Out.Int
CALL 2
CONST 7
GLOBAL tSystolic07.%3
GLOBAL Out.String
CALL 2
!   Out.Int(chid, 0); Out.String("."); PrintVal(x); Out.Ln
CONST 0
LDLW 12
GLOBAL Out.Int
CALL 2
CONST 2
GLOBAL tSystolic07.%8
GLOBAL Out.String
CALL 2
LDLW 16
GLOBAL tSystolic07.PrintVal
CALL 1
GLOBAL Out.Ln
CALL 0
RETURN
END

PROC tSystolic07.Print 0 3 0
! PROCEDURE Print(x: INTEGER);
!   Out.String("Time "); Out.Int(t, 0); Out.String(": print ");
CONST 6
GLOBAL tSystolic07.%2
GLOBAL Out.String
CALL 2
CONST 0
LDGW tSystolic07.t
GLOBAL Out.Int
CALL 2
CONST 9
GLOBAL tSystolic07.%4
GLOBAL Out.String
CALL 2
!   PrintVal(x); Out.Ln
LDLW 12
GLOBAL tSystolic07.PrintVal
CALL 1
GLOBAL Out.Ln
CALL 0
RETURN
END

PROC tSystolic07.Max 4 3 0
! PROCEDURE Max(x, y: INTEGER): INTEGER;
!   IF x >= y THEN r := x ELSE r := y END
LDLW 12
LDLW 16
JLT 14
LDLW 12
STLW -4
JUMP 12
LABEL 14
LDLW 16
STLW -4
LABEL 12
! RETURN r
LDLW -4
RETURNW
END

PROC tSystolic07.Min 4 3 0
! PROCEDURE Min(x, y: INTEGER): INTEGER;
!   IF x <= y THEN r := x ELSE r := y END
LDLW 12
LDLW 16
JGT 17
LDLW 12
STLW -4
JUMP 15
LABEL 17
LDLW 16
STLW -4
LABEL 15
! RETURN r
LDLW -4
RETURNW
END

PROC tSystolic07.Init 0 3 0x00d00001
! PROCEDURE Init(self: Process; pid: INTEGER; left, right: Channel; 
!   self.pid := pid;
LDLW 16
LDLW 12
NCHECK 70
STOREW
!   self.left := left; self.right := right;
LDLW 20
LDLW 12
NCHECK 71
STNW 16
LDLW 24
LDLW 12
NCHECK 71
STNW 20
!   self.step := step;
LDLW 32
GCHECK 72
LDLW 28
LDLW 12
NCHECK 72
STNW 28
!   self.status := Ready;
CONST 1
LDLW 12
NCHECK 73
STNW 4
!   self.pc := 0
CONST 0
LDLW 12
NCHECK 74
STNW 8
RETURN
END

PROC tSystolic07.Run 0 3 0x00100001
! PROCEDURE Run(self: Process);
LABEL 18
!   WHILE (self.status = Ready) OR (self.status = Waking) DO
LDLW 12
NCHECK 80
LDNW 4
CONST 1
JEQ 19
LDLW 12
NCHECK 80
LDNW 4
CONST 3
JNEQ 20
LABEL 19
!     self.next := self.pc+1;
LDLW 12
NCHECK 81
LDNW 8
INC
LDLW 12
NCHECK 81
STNW 12
!     self.step(self);
LDLW 12
LDLW 12
NCHECK 82
LDNW 28
NCHECK 82
CALL 1
!     self.pc := self.next
LDLW 12
NCHECK 83
LDNW 12
LDLW 12
NCHECK 83
STNW 8
JUMP 18
LABEL 20
RETURN
END

PROC tSystolic07.Get 0 3 0x00300001
! PROCEDURE Get(self: Process; VAR x: INTEGER);
!   CASE self.status OF
LDLW 12
NCHECK 90
LDNW 4
DEC
JCASE 3
CASEL 24
CASEL 22
CASEL 25
JUMP 22
LABEL 24
!         self.left.receiver := self;
LDLW 12
LDLW 12
NCHECK 93
LDNW 16
NCHECK 93
STNW 8
!         self.status := Sleeping;
CONST 2
LDLW 12
NCHECK 94
STNW 4
!         self.next := self.pc (* Try input again when we wake *)
LDLW 12
NCHECK 95
LDNW 8
LDLW 12
NCHECK 95
STNW 12
RETURN
LABEL 25
!         x := self.buf;
LDLW 12
NCHECK 98
LDNW 24
LDLW 16
STOREW
!         self.status := Ready
CONST 1
LDLW 12
NCHECK 99
STNW 4
RETURN
LABEL 22
ERROR E_CASE 90
RETURN
END

PROC tSystolic07.Put 0 3 0x00100001
! PROCEDURE Put(self: Process; x: INTEGER);
!   CASE self.status OF
LDLW 12
NCHECK 106
LDNW 4
DEC
JCASE 3
CASEL 28
CASEL 26
CASEL 29
JUMP 26
LABEL 28
!         self.buf := x;
LDLW 16
LDLW 12
NCHECK 108
STNW 24
!         self.right.sender := self;
LDLW 12
LDLW 12
NCHECK 109
LDNW 20
NCHECK 109
STNW 4
!         self.status := Sleeping;
CONST 2
LDLW 12
NCHECK 110
STNW 4
!         self.next := self.pc
LDLW 12
NCHECK 111
LDNW 8
LDLW 12
NCHECK 111
STNW 12
RETURN
LABEL 29
!         self.status := Ready
CONST 1
LDLW 12
NCHECK 113
STNW 4
RETURN
LABEL 26
ERROR E_CASE 106
RETURN
END

PROC tSystolic07.Goto 0 3 0x00100001
! PROCEDURE Goto(self: Process; lab: INTEGER);
!   self.next := lab
LDLW 16
LDLW 12
NCHECK 120
STNW 12
RETURN
END

PROC tSystolic07.Halt 0 3 0x00100001
! PROCEDURE Halt(self: Process);
!   self.status := Dead
CONST 0
LDLW 12
NCHECK 126
STNW 4
RETURN
END

PROC tSystolic07.MakeChannel 4 4 0x00010001
! PROCEDURE MakeChannel(chid: INTEGER): Channel;
!   NEW(ch); InitChan(ch, chid); RETURN ch
CONST 12
GLOBAL tSystolic07.ChanRec
LOCAL -4
GLOBAL NEW
CALL 3
LDLW 12
LDLW -4
GLOBAL tSystolic07.InitChan
CALL 2
LDLW -4
RETURNW
END

PROC tSystolic07.InitChan 0 4 0x00100001
! PROCEDURE InitChan(self: Channel; chid: INTEGER);
!   self.chid := chid;
LDLW 16
LDLW 12
NCHECK 138
STOREW
!   self.sender := NIL; self.receiver := NIL
CONST 0
LDLW 12
NCHECK 139
STNW 4
CONST 0
LDLW 12
NCHECK 139
STNW 8
RETURN
END

PROC tSystolic07.Eval 4 4 0x00100001
! PROCEDURE Eval(self: Channel);
!   IF (self.sender # NIL) & (self.receiver # NIL) THEN
LDLW 12
NCHECK 146
LDNW 4
JEQZ 32
LDLW 12
NCHECK 146
LDNW 8
JEQZ 32
!     x := self.sender.buf;
LDLW 12
NCHECK 148
LDNW 4
NCHECK 148
LDNW 24
STLW -4
!     Trace(self.chid, x);
LDLW -4
LDLW 12
NCHECK 149
LOADW
GLOBAL tSystolic07.Trace
CALL 2
!     self.receiver.buf := x;
LDLW -4
LDLW 12
NCHECK 150
LDNW 8
NCHECK 150
STNW 24
!     self.sender.status := Waking; self.receiver.status := Waking;
CONST 3
LDLW 12
NCHECK 151
LDNW 4
NCHECK 151
STNW 4
CONST 3
LDLW 12
NCHECK 151
LDNW 8
NCHECK 151
STNW 4
!     self.sender := NIL; self.receiver := NIL
CONST 0
LDLW 12
NCHECK 152
STNW 4
CONST 0
LDLW 12
NCHECK 152
STNW 8
LABEL 32
RETURN
END

PROC tSystolic07.MakeInjector 4 7 0x00210001
! PROCEDURE MakeInjector(pid: INTEGER; right: Channel): Injector;
!   NEW(p); Init(p, pid, NIL, right, StepInj); RETURN p
CONST 40
GLOBAL tSystolic07.%5
LOCAL -4
GLOBAL NEW
CALL 3
CONST 0
GLOBAL tSystolic07.StepInj
LDLW 16
CONST 0
LDLW 12
LDLW -4
GLOBAL tSystolic07.Init
CALL 6
LDLW -4
RETURNW
END

PROC tSystolic07.StepInj 4 7 0x00110001
! PROCEDURE StepInj(self0: Process);
!   self := self0(Injector);
LDLW 12
DUP 0
NCHECK 185
LDNW -4
GLOBAL tSystolic07.%5
TYPETEST 1
JUMPT 34
ERROR E_CAST 185
LABEL 34
STLW -4
!   CASE self.pc OF
LDLW -4
NCHECK 186
LDNW 8
JCASE 7
CASEL 37
CASEL 38
CASEL 39
CASEL 40
CASEL 41
CASEL 42
CASEL 43
JUMP 35
LABEL 37
!       0: self.r := 0
CONST 0
LDLW -4
NCHECK 187
STNW 32
RETURN
LABEL 38
!     | 1: IF self.r = N THEN Goto(self, 5) END
LDLW -4
NCHECK 188
LDNW 32
CONST 10
JNEQ 36
CONST 5
LDLW -4
GLOBAL tSystolic07.Goto
CALL 2
RETURN
LABEL 39
!     | 2: self.x := Random.Roll(100); 
CONST 100
GLOBAL Random.Roll
CALLW 1
LDLW -4
NCHECK 189
STNW 36
RETURN
LABEL 40
!     | 3: Put(self, self.x);
LDLW -4
NCHECK 190
LDNW 36
LDLW -4
GLOBAL tSystolic07.Put
CALL 2
RETURN
LABEL 41
!     | 4: INC(self.r); Goto(self, 1)
LDLW -4
NCHECK 191
DUP 0
LDNW 32
INC
SWAP
STNW 32
CONST 1
LDLW -4
GLOBAL tSystolic07.Goto
CALL 2
RETURN
LABEL 42
!     | 5: Put(self, INF)
CONST 859
LDLW -4
GLOBAL tSystolic07.Put
CALL 2
RETURN
LABEL 43
!     | 6: Halt(self)
LDLW -4
GLOBAL tSystolic07.Halt
CALL 1
RETURN
LABEL 35
ERROR E_CASE 186
LABEL 36
RETURN
END

PROC tSystolic07.MakeComparator 4 7 0x00610001
! PROCEDURE MakeComparator(pid: INTEGER; left, right: Channel): Comparator;
!   NEW(p); Init(p, pid, left, right, StepComp); RETURN p
CONST 40
GLOBAL tSystolic07.%6
LOCAL -4
GLOBAL NEW
CALL 3
CONST 0
GLOBAL tSystolic07.StepComp
LDLW 20
LDLW 16
LDLW 12
LDLW -4
GLOBAL tSystolic07.Init
CALL 6
LDLW -4
RETURNW
END

PROC tSystolic07.StepComp 4 7 0x00110001
! PROCEDURE StepComp(self0: Process);
!   self := self0(Comparator);
LDLW 12
DUP 0
NCHECK 210
LDNW -4
GLOBAL tSystolic07.%6
TYPETEST 1
JUMPT 47
ERROR E_CAST 210
LABEL 47
STLW -4
!   CASE self.pc OF
LDLW -4
NCHECK 211
LDNW 8
JCASE 8
CASEL 50
CASEL 51
CASEL 52
CASEL 53
CASEL 54
CASEL 55
CASEL 56
CASEL 57
JUMP 48
LABEL 50
!       0: Get(self, self.x)
LDLW -4
NCHECK 212
CONST 32
PLUSA
LDLW -4
GLOBAL tSystolic07.Get
CALL 2
RETURN
LABEL 51
!     | 1: Get(self, self.y)
LDLW -4
NCHECK 213
CONST 36
PLUSA
LDLW -4
GLOBAL tSystolic07.Get
CALL 2
RETURN
LABEL 52
!     | 2: IF self.y = INF THEN Goto(self, 5) END
LDLW -4
NCHECK 214
LDNW 36
CONST 859
JNEQ 49
CONST 5
LDLW -4
GLOBAL tSystolic07.Goto
CALL 2
RETURN
LABEL 53
!     | 3: Put(self, Min(self.x, self.y))
LDLW -4
NCHECK 215
LDNW 36
LDLW -4
NCHECK 215
LDNW 32
GLOBAL tSystolic07.Min
CALLW 2
LDLW -4
GLOBAL tSystolic07.Put
CALL 2
RETURN
LABEL 54
!     | 4: self.x := Max(self.x, self.y); Goto(self, 1)
LDLW -4
NCHECK 216
LDNW 36
LDLW -4
NCHECK 216
LDNW 32
GLOBAL tSystolic07.Max
CALLW 2
LDLW -4
NCHECK 216
STNW 32
CONST 1
LDLW -4
GLOBAL tSystolic07.Goto
CALL 2
RETURN
LABEL 55
!     | 5: Put(self, self.x)
LDLW -4
NCHECK 217
LDNW 32
LDLW -4
GLOBAL tSystolic07.Put
CALL 2
RETURN
LABEL 56
!     | 6: Put(self, INF)
CONST 859
LDLW -4
GLOBAL tSystolic07.Put
CALL 2
RETURN
LABEL 57
!     | 7: Halt(self)
LDLW -4
GLOBAL tSystolic07.Halt
CALL 1
RETURN
LABEL 48
ERROR E_CASE 211
LABEL 49
RETURN
END

PROC tSystolic07.MakeCollector 4 7 0x00210001
! PROCEDURE MakeCollector(pid: INTEGER; left: Channel): Collector;
!   NEW(p); Init(p, pid, left, NIL, StepColl); RETURN p
CONST 36
GLOBAL tSystolic07.%7
LOCAL -4
GLOBAL NEW
CALL 3
CONST 0
GLOBAL tSystolic07.StepColl
CONST 0
LDLW 16
LDLW 12
LDLW -4
GLOBAL tSystolic07.Init
CALL 6
LDLW -4
RETURNW
END

PROC tSystolic07.StepColl 4 7 0x00110001
! PROCEDURE StepColl(self0: Process);
!   self := self0(Collector);
LDLW 12
DUP 0
NCHECK 236
LDNW -4
GLOBAL tSystolic07.%7
TYPETEST 1
JUMPT 61
ERROR E_CAST 236
LABEL 61
STLW -4
!   CASE self.pc OF
LDLW -4
NCHECK 237
LDNW 8
JCASE 4
CASEL 64
CASEL 65
CASEL 66
CASEL 67
JUMP 62
LABEL 64
!       0: Get(self, self.x)
LDLW -4
NCHECK 238
CONST 32
PLUSA
LDLW -4
GLOBAL tSystolic07.Get
CALL 2
RETURN
LABEL 65
!     | 1: IF self.x = INF THEN Goto(self, 3) END
LDLW -4
NCHECK 239
LDNW 32
CONST 859
JNEQ 63
CONST 3
LDLW -4
GLOBAL tSystolic07.Goto
CALL 2
RETURN
LABEL 66
!     | 2: Print(self.x); Goto(self, 0)
LDLW -4
NCHECK 240
LDNW 32
GLOBAL tSystolic07.Print
CALL 1
CONST 0
LDLW -4
GLOBAL tSystolic07.Goto
CALL 2
RETURN
LABEL 67
!     | 3: Halt(self)
LDLW -4
GLOBAL tSystolic07.Halt
CALL 1
RETURN
LABEL 62
ERROR E_CASE 237
LABEL 63
RETURN
END

PROC tSystolic07.Build 4 7 0
! PROCEDURE Build;
!   FOR i := 0 TO N DO chan[i] := MakeChannel(i) END;
CONST 0
STLW -4
LABEL 71
LDLW -4
CONST 10
JGT 72
LDLW -4
GLOBAL tSystolic07.MakeChannel
CALLW 1
GLOBAL tSystolic07.chan
LDLW -4
CONST 11
BOUND 256
STIW
INCL -4
JUMP 71
LABEL 72
!   proc[0] := MakeInjector(0, chan[0]);
LDGW tSystolic07.chan
CONST 0
GLOBAL tSystolic07.MakeInjector
CALLW 2
STGW tSystolic07.proc
!   FOR i := 1 TO N DO proc[i] := MakeComparator(i, chan[i-1], chan[i]) END;
CONST 1
STLW -4
LABEL 73
LDLW -4
CONST 10
JGT 74
GLOBAL tSystolic07.chan
LDLW -4
CONST 11
BOUND 259
LDIW
GLOBAL tSystolic07.chan
LDLW -4
DEC
CONST 11
BOUND 259
LDIW
LDLW -4
GLOBAL tSystolic07.MakeComparator
CALLW 3
GLOBAL tSystolic07.proc
LDLW -4
CONST 12
BOUND 259
STIW
INCL -4
JUMP 73
LABEL 74
!   proc[N+1] := MakeCollector(N+1, chan[N]);
GLOBAL tSystolic07.chan
LDNW 40
CONST 11
GLOBAL tSystolic07.MakeCollector
CALLW 2
GLOBAL tSystolic07.proc
STNW 44
RETURN
END

PROC tSystolic07.RunSim 4 7 0
! PROCEDURE RunSim;
!   t := 0;
CONST 0
STGW tSystolic07.t
LABEL 75
!   WHILE proc[N+1].status # Dead DO
GLOBAL tSystolic07.proc
LDNW 44
NCHECK 268
LDNW 4
JEQZ 77
!     FOR i := 0 TO N+1 DO Run(proc[i]) END;
CONST 0
STLW -4
LABEL 78
LDLW -4
CONST 11
JGT 79
GLOBAL tSystolic07.proc
LDLW -4
CONST 12
BOUND 269
LDIW
GLOBAL tSystolic07.Run
CALL 1
INCL -4
JUMP 78
LABEL 79
!     t := t+1;
LDGW tSystolic07.t
INC
STGW tSystolic07.t
!     FOR i := 0 TO N DO Eval(chan[i]) END
CONST 0
STLW -4
LABEL 80
LDLW -4
CONST 10
JGT 75
GLOBAL tSystolic07.chan
LDLW -4
CONST 11
BOUND 271
LDIW
GLOBAL tSystolic07.Eval
CALL 1
INCL -4
JUMP 80
LABEL 77
RETURN
END

PROC tSystolic07.%main 0 7 0
!   Build;
GLOBAL tSystolic07.Build
CALL 0
!   RunSim
GLOBAL tSystolic07.RunSim
CALL 0
RETURN
END

! Global variables
GLOVAR tSystolic07.t 4
GLOVAR tSystolic07.proc 48
GLOVAR tSystolic07.chan 44

! Pointer map
DEFINE tSystolic07.%gcmap
WORD GC_BASE
WORD tSystolic07.proc
WORD GC_MAP
WORD 0x00001fff
WORD GC_BASE
WORD tSystolic07.chan
WORD GC_MAP
WORD 0x00000fff
WORD GC_END

! String "INF"
DEFINE tSystolic07.%1
STRING 494E4600

! String "Time "
DEFINE tSystolic07.%2
STRING 54696D652000

! String ": chan"
DEFINE tSystolic07.%3
STRING 3A206368616E00

! String ": print "
DEFINE tSystolic07.%4
STRING 3A207072696E742000

! String "."
DEFINE tSystolic07.%8
STRING 2E00

! Descriptor for ProcRec
DEFINE tSystolic07.ProcRec
WORD 0x00000061
WORD 0
WORD tSystolic07.ProcRec.%anc

DEFINE tSystolic07.ProcRec.%anc
WORD tSystolic07.ProcRec

! Descriptor for ChanRec
DEFINE tSystolic07.ChanRec
WORD 0x0000000d
WORD 0
WORD tSystolic07.ChanRec.%anc

DEFINE tSystolic07.ChanRec.%anc
WORD tSystolic07.ChanRec

! Descriptor for *anon*
DEFINE tSystolic07.%5
WORD 0x00000061
WORD 1
WORD tSystolic07.%5.%anc

DEFINE tSystolic07.%5.%anc
WORD tSystolic07.ProcRec
WORD tSystolic07.%5

! Descriptor for *anon*
DEFINE tSystolic07.%6
WORD 0x00000061
WORD 1
WORD tSystolic07.%6.%anc

DEFINE tSystolic07.%6.%anc
WORD tSystolic07.ProcRec
WORD tSystolic07.%6

! Descriptor for *anon*
DEFINE tSystolic07.%7
WORD 0x00000061
WORD 1
WORD tSystolic07.%7.%anc

DEFINE tSystolic07.%7.%anc
WORD tSystolic07.ProcRec
WORD tSystolic07.%7

! End of file
]]*)
