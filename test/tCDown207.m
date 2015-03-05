MODULE tCDown207;

(* CountDown in Oberon.  From an idea by Stephen Williams;
   re-implementation in Oberon and tuning by Mike Spivey *)

IMPORT Out, Conv, Strings, Bit, GC;

CONST 
  MAX = 10;			(* Maximum number of inputs *)
  POWMAX = 1024;		(* $2^{\hbox{\sci max}}$ *)
  HSIZE = 20000;		(* Size of hash table *)

CONST				(* Operator symbols *)
  Const = 0; Plus = 1; Minus = 2; Times = 3; Divide = 4;

(* Each expression formed is recorded as a `blob'.  These blobs are
   linked together in three ways: a blob that's labelled with a binary
   operator is linked to its left and right operands in a binary tree
   structure.  Also, each blob is put in a linked list with all others
   that use the same inputs.  Finally, there is a chained hash table
   that allows us to find all blobs with a certain value. *)

TYPE blobptr = POINTER TO blob;
  blob = 
    RECORD
      op: INTEGER;		(* Operator *)
      left, right: blobptr;	(* Left and right operands *)
      val: INTEGER;		(* Value of expression *)
      used: INTEGER;		(* Bitmap of inputs used *)
      next: blobptr;		(* Next blob with same inputs *)
      hlink: blobptr;		(* Next blob in hash chain *)
    END;

(* The binary tree structure of blobs is used by |Grind|, which
   converts an expression to printed form.  The output is simplified by
   omitting brackets where they are unnecessary because of the
   priority and associativity of operators.  Thus both of the
   expressions \verb|(1+2)+3| and \verb|1+(2+3)| will be shown without
   brackets.  The hacky string constants are a way of getting round
   the lack of proper array constants in Oberon *)

CONST
  sym = "?+-*/";		(* Character symbol for each operator *)
  pri = "01122";		(* Priorities of the operators *)
  rpri = "01223";		(* Priorities for right operands *)

(* |Grind| -- convert expression to printed form *)
PROCEDURE Grind(e0: blobptr; VAR buf: ARRAY OF CHAR);
  VAR pos: INTEGER;			(* Current position in |buf| *)

  PROCEDURE Put(c: CHAR);
  BEGIN
    buf[pos] := c; pos := pos+1
  END Put;

  PROCEDURE Walk(e: blobptr; p: INTEGER);
    VAR j, kp, rp: INTEGER; cbuf: ARRAY 10 OF CHAR;
  BEGIN
    IF e.op = Const THEN
      (* A constant *)
      Conv.ConvInt(e.val, cbuf);
      j := 0;
      WHILE cbuf[j] # 0X DO Put(cbuf[j]); j := j+1 END
    ELSE
      (* A binary operator *)
      kp := ORD(pri[e.op]) - ORD('0');
      rp := ORD(rpri[e.op]) - ORD('0');
      IF kp < p THEN Put('(') END;
      Walk(e.left, kp);
      Put(' '); Put(sym[e.op]); Put(' ');
      Walk(e.right, rp);
      IF kp < p THEN Put(')') END
    END
  END Walk;

BEGIN
  pos := 0;
  Walk(e0, 1);
  Put(0X)
END Grind;

(* Sets of input numbers are represented by bitmaps, i.e. integers in
   the range $[0\ddot2^N)$ in which the one bits indicate which
   numbers are present.  The array entry |pool[s]| shows all the expressions
   that have been created using the set of inputs |s|. *)

VAR pool: ARRAY POWMAX OF blobptr;

(* For each possible value |val|, we keep track of all the expressions
   with value |val| that have been created: they are kept in a linked
   list starting at |htable[val MOD HSIZE]|, together (of course) with
   others that hash to the same bucket.  We've chosen |HSIZE| large
   enough that such collisions will rarely happen. 
   The purpose of this hash table is to make it easy to avoid creating
   a `useless' expression if another with the same value already
   exists and uses no inputs that the new one would not use.  This
   speeds up the search immensely. *)

VAR htable: POINTER TO ARRAY HSIZE OF blobptr;

(* As we generate expressions, we keep track of the best answer seen
   so far: an expression that comes closest to the
   target, and of the expressions that are that close, the one that is
   the shortest when printed. *)

VAR
  target: INTEGER;		(* The target number *)
  temp, best: ARRAY 80 OF CHAR;	(* Latest expression, and best found so far *)
  bestval, bestdist: INTEGER;	(* Value and distance of |best| from |target| *)

(* |Add| -- create a new expression if it is not useless *)
PROCEDURE Add(op: INTEGER; p, q: blobptr; val, used: INTEGER);
  VAR dist: INTEGER; r: blobptr; useless: BOOLEAN;
BEGIN
  (* Return immediately if useless *)
  r := htable[val MOD HSIZE]; useless := FALSE;
  WHILE (r # NIL) & ~useless DO
    IF (r.val = val) & (Bit.And(r.used, Bit.Not(used)) = 0) THEN
      useless := TRUE
    END;
    r := r.hlink
  END;

IF ~useless THEN
  (* Create the expression and add it to |pool| and |htable| *)
  NEW(r);
  r.op := op; r.left := p; r.right := q; r.val := val; r.used := used;
  r.next := pool[used]; pool[used] := r;
  r.hlink := htable[val MOD HSIZE]; htable[val MOD HSIZE] := r;

  (* See if the new expression comes near the target *)
  dist := ABS(val - target);
  IF dist <= bestdist THEN
    (* At least as close as before *)
    Grind(r, temp);
    IF (dist < bestdist) 
        OR (Strings.Length(temp) < Strings.Length(best)) THEN
      (* Actually closer, or anyway shorter *)
      bestval := val; bestdist := dist;
      COPY(temp, best)
    END
  END
END
END Add;

(* The |Combine| procedure combines the contents of |pool[r]| with the
contents of |pool[s]| using every possible operator.  The results are
entered into the pool for the set union of |r| and |s|.
To speed the search, we do not allow expressions of the form $E_1+E_2$
where the value of $E_1$ is smaller than the value of $E_2$; the
equivalent expression $E_2+E_1$ renders this one useless anyway *)

(* |Combine| -- combine each expression in |pool[r]| with each in |pool[s]| *)
PROCEDURE Combine(r, s: INTEGER);
  VAR p, q: blobptr; used: INTEGER;
BEGIN
  used := Bit.Or(r, s);
  p := pool[r];
  WHILE p # NIL DO
    q := pool[s];
    WHILE q # NIL DO
      IF p.val >= q.val THEN
	Add(Plus, p, q, p.val+q.val, used);
	IF p.val > q.val THEN Add(Minus, p, q, p.val-q.val, used) END;
	Add(Times, p, q, p.val*q.val, used);
	IF (q.val > 0) & (p.val MOD q.val = 0) THEN
	  Add(Divide, p, q, p.val DIV q.val, used)
        END
      END;
      q := q.next
    END;
    p := p.next
  END
END Combine;

(* The search algorithm works by starting with just the input numbers,
and successively forming all expressions
using 2, 3,~\dots input numbers.
Each expression with $i$ inputs can be obtained by combining two
expressions that use $j$ and $k$ inputs, where $j+k=i$, and the two
expressions use disjoint sets of inputs.  Since the expressions are
divided into pools according to the set of inputs they use, at the |i|'th
stage we must combine each pool |r| with each pool |s| such that
|ones[r]+ones[s]=i| and |r| and |s| are disjoint. *)

(* |Search| -- search for all ways to reach the target. *)
PROCEDURE Search(n: INTEGER; draw: ARRAY OF INTEGER);
  VAR 
    i, r, s, t: INTEGER;
    ones: ARRAY POWMAX OF INTEGER; (* |ones[i]| is no. of 1 bits in |i|. *)
BEGIN
  (* Set up the table in |ones|.  For each |i|, we should have
     $"ones"[2^i..2^{i+1}) = "ones"[0..2^i) + 1$. *)
  ones[0] := 0; t := 1;
  FOR i := 0 TO n-1 DO
    FOR r := 0 TO t-1 DO ones[t+r] := ones[r]+1 END;
    t := 2*t
  END;

  (* Empty the hash table and pools *)
  FOR i := 0 TO HSIZE-1 DO htable[i] := NIL END;
  FOR r := 0 TO t-1 DO pool[r] := NIL END;

  (* Plant the original numbers as seeds, and set $t = 2^n$. *)
  t := 1;
  FOR i := 0 TO n-1 DO
    Add(Const, NIL, NIL, draw[i], t);
    t := 2*t
  END;

  (* Combine using up to |n-1| operations. *)
  bestdist := 100000;
  FOR i := 2 TO n DO 
    (* Combine all disjoint pairs of pools that use a total of |i| inputs *)
    FOR r := 1 TO t-1 DO
      FOR s := 1 TO t-1 DO
	IF (ones[r] + ones[s] = i) & (Bit.And(r, s) = 0) THEN
	  Combine(r, s)
        END
      END
    END
  END
END Search;

(*
(* |RandChoice| -- randomly choose six numbers *)
PROCEDURE RandChoice(VAR draw: ARRAY OF INTEGER);
  VAR i, j: INTEGER;
BEGIN
  (* We want 5 numbers from 1, 1, 2, 2, ..., 10, 10, with each number equally 
     likely to be chosen. The laws of conditional probablility show how to do 
     it: at each stage, we must select |(5-j)| numbers from the |(20-i)| that 
     remain, so the first of them is selected with that probability. *)
  FOR i := 0 TO 19 DO
    (* Choose |i/2+1| with probability |(5-j)/(20-i)| *)
    IF Random.Roll(20-i) < 5-j THEN
      draw[j] := i DIV 2 + 1; j := j+1
    END
  END;
  draw[5] := 25 * (Random.Roll(4) + 1);
  target := 101 + Random.Roll(899)
END RandChoice;
*)

(* |Main| -- main program *)
PROCEDURE Main;
  VAR 
    i, n: INTEGER; 
    (* buf: ARRAY 20 OF CHAR; *)
    draw: ARRAY MAX OF INTEGER;
BEGIN
  n := 6;
  draw[0] := 100; draw[1] := 4; draw[2] := 25; 
  draw[3] := 50; draw[4] := 2; draw[5] := 8; 
  target := 999;

(*
  IF Args.argc = 1 THEN
    (* Random problem *)
    Random.Randomize;
    n := 6;
    RandChoice(draw);
  ELSIF (Args.argc >= 3) & (Args.argc < MAX+1) THEN
    (* Problem specified on command line *)
    n := Args.argc-2;
    FOR i := 0 TO n-1 DO
      Args.GetArg(i+1, buf);
      draw[i] := Conv.IntVal(buf)
    END;
    Args.GetArg(Args.argc-1, buf);
    target := Conv.IntVal(buf);
  ELSE
    Out.String("usage: countdown [x1 x2 ... xn target]");
    Out.Ln;
    HALT(1)
  END;
*)

  (* Print input numbers (ascending if we chose them). *)
  Out.String("To make "); Out.Int(target, 0); Out.String(" from");
  FOR i := 0 TO n-1 DO Out.Char(' '); Out.Int(draw[i], 0) END;
  Out.Char(':'); Out.Ln;

  (* Find the best solution *)
  Search(n, draw);

  (* Print it. *)
  Out.Ln; Out.String("  "); Out.String(best); 
  Out.String(" = "); Out.Int(bestval, 0);   
  IF bestdist > 0 THEN 
    Out.String(" (off by "); Out.Int(bestdist, 0); Out.Char(')')
  END;
  Out.Ln; Out.Ln
END Main;

BEGIN
  GC.Debug("gs");
  NEW(htable);
  Main;
  htable := NIL;
  GC.Collect;
  Out.Ln
END tCDown207.

(*<<
To make 999 from 100 4 25 50 2 8:

  (50 - (100 + 4) / 8) * (25 + 2) = 999

[gc[ex]]
>>*)

(*[[
!! SYMFILE #tCDown207 STAMP #tCDown207.%main 1
!! END STAMP
!! 
MODULE tCDown207 STAMP 0
IMPORT Out STAMP
IMPORT Conv STAMP
IMPORT Strings STAMP
IMPORT Bit STAMP
IMPORT GC STAMP
ENDHDR

PROC tCDown207.%11.Put 4 5 0
!   PROCEDURE Put(c: CHAR);
SAVELINK
!     buf[pos] := c; pos := pos+1
LDLC 12
LDEW 16
LDEW -4
LDEW 20
BOUND 53
STIC
LDEW -4
INC
STEW -4
RETURN
END

PROC tCDown207.%12.Walk 28 5 0x00100001
!   PROCEDURE Walk(e: blobptr; p: INTEGER);
SAVELINK
!     IF e.op = Const THEN
LDLW 12
NCHECK 59
LOADW
JNEQZ 21
!       Conv.ConvInt(e.val, cbuf);
CONST 10
LOCAL -26
LDLW 12
NCHECK 61
LDNW 12
GLOBAL Conv.ConvInt
CALL 3
!       j := 0;
CONST 0
STLW -8
LABEL 22
!       WHILE cbuf[j] # 0X DO Put(cbuf[j]); j := j+1 END
LOCAL -26
LDLW -8
CONST 10
BOUND 63
LDIC
JEQZ 19
LOCAL -26
LDLW -8
CONST 10
BOUND 63
LDIC
ALIGNC
LDLW -4
LINK
GLOBAL tCDown207.%11.Put
CALL 1
INCL -8
JUMP 22
LABEL 21
!       kp := ORD(pri[e.op]) - ORD('0');
GLOBAL tCDown207.%2
LDLW 12
NCHECK 66
LOADW
CONST 6
BOUND 66
LDIC
CONST 48
MINUS
STLW -12
!       rp := ORD(rpri[e.op]) - ORD('0');
GLOBAL tCDown207.%3
LDLW 12
NCHECK 67
LOADW
CONST 6
BOUND 67
LDIC
CONST 48
MINUS
STLW -16
!       IF kp < p THEN Put('(') END;
LDLW -12
LDLW 16
JGEQ 16
CONST 40
ALIGNC
LDLW -4
LINK
GLOBAL tCDown207.%11.Put
CALL 1
LABEL 16
!       Walk(e.left, kp);
LDLW -12
LDLW 12
NCHECK 69
LDNW 4
LDLW -4
LINK
GLOBAL tCDown207.%12.Walk
CALL 2
!       Put(' '); Put(sym[e.op]); Put(' ');
CONST 32
ALIGNC
LDLW -4
LINK
GLOBAL tCDown207.%11.Put
CALL 1
GLOBAL tCDown207.%1
LDLW 12
NCHECK 70
LOADW
CONST 6
BOUND 70
LDIC
ALIGNC
LDLW -4
LINK
GLOBAL tCDown207.%11.Put
CALL 1
CONST 32
ALIGNC
LDLW -4
LINK
GLOBAL tCDown207.%11.Put
CALL 1
!       Walk(e.right, rp);
LDLW -16
LDLW 12
NCHECK 71
LDNW 8
LDLW -4
LINK
GLOBAL tCDown207.%12.Walk
CALL 2
!       IF kp < p THEN Put(')') END
LDLW -12
LDLW 16
JGEQ 19
CONST 41
ALIGNC
LDLW -4
LINK
GLOBAL tCDown207.%11.Put
CALL 1
LABEL 19
RETURN
END

PROC tCDown207.Grind 4 5 0x00300001
! PROCEDURE Grind(e0: blobptr; VAR buf: ARRAY OF CHAR);
!   pos := 0;
CONST 0
STLW -4
!   Walk(e0, 1);
CONST 1
LDLW 12
LOCAL 0
LINK
GLOBAL tCDown207.%12.Walk
CALL 2
!   Put(0X)
CONST 0
ALIGNC
LOCAL 0
LINK
GLOBAL tCDown207.%11.Put
CALL 1
RETURN
END

PROC tCDown207.Add 12 5 0x00608001
! PROCEDURE Add(op: INTEGER; p, q: blobptr; val, used: INTEGER);
!   r := htable[val MOD HSIZE]; useless := FALSE;
LDGW tCDown207.htable
NCHECK 116
LDLW 24
CONST 20000
MOD
CONST 20000
BOUND 116
LDIW
STLW -8
CONST 0
STLC -9
LABEL 25
!   WHILE (r # NIL) & ~useless DO
LDLW -8
JEQZ 27
LDLC -9
JUMPT 27
!     IF (r.val = val) & (Bit.And(r.used, Bit.Not(used)) = 0) THEN
LDLW -8
NCHECK 118
LDNW 12
LDLW 24
JNEQ 30
LDLW 28
GLOBAL Bit.Not
CALLW 1
LDLW -8
NCHECK 118
LDNW 16
GLOBAL Bit.And
CALLW 2
JNEQZ 30
!       useless := TRUE
CONST 1
STLC -9
LABEL 30
!     r := r.hlink
LDLW -8
NCHECK 121
LDNW 24
STLW -8
JUMP 25
LABEL 27
! IF ~useless THEN
LDLC -9
JUMPT 35
!   NEW(r);
CONST 28
GLOBAL tCDown207.blob
LOCAL -8
GLOBAL NEW
CALL 3
!   r.op := op; r.left := p; r.right := q; r.val := val; r.used := used;
LDLW 12
LDLW -8
NCHECK 127
STOREW
LDLW 16
LDLW -8
NCHECK 127
STNW 4
LDLW 20
LDLW -8
NCHECK 127
STNW 8
LDLW 24
LDLW -8
NCHECK 127
STNW 12
LDLW 28
LDLW -8
NCHECK 127
STNW 16
!   r.next := pool[used]; pool[used] := r;
GLOBAL tCDown207.pool
LDLW 28
CONST 1024
BOUND 128
LDIW
LDLW -8
NCHECK 128
STNW 20
LDLW -8
GLOBAL tCDown207.pool
LDLW 28
CONST 1024
BOUND 128
STIW
!   r.hlink := htable[val MOD HSIZE]; htable[val MOD HSIZE] := r;
LDGW tCDown207.htable
NCHECK 129
LDLW 24
CONST 20000
MOD
CONST 20000
BOUND 129
LDIW
LDLW -8
NCHECK 129
STNW 24
LDLW -8
LDGW tCDown207.htable
NCHECK 129
LDLW 24
CONST 20000
MOD
CONST 20000
BOUND 129
STIW
!   dist := ABS(val - target);
LDLW 24
LDGW tCDown207.target
MINUS
GLOBAL ABSINT
CALLW 1
STLW -4
!   IF dist <= bestdist THEN
LDLW -4
LDGW tCDown207.bestdist
JGT 35
!     Grind(r, temp);
CONST 80
GLOBAL tCDown207.temp
LDLW -8
GLOBAL tCDown207.Grind
CALL 3
!     IF (dist < bestdist) 
LDLW -4
LDGW tCDown207.bestdist
JLT 40
CONST 80
GLOBAL tCDown207.temp
GLOBAL Strings.Length
CALLW 2
CONST 80
GLOBAL tCDown207.best
GLOBAL Strings.Length
CALLW 2
JGEQ 35
LABEL 40
!       bestval := val; bestdist := dist;
LDLW 24
STGW tCDown207.bestval
LDLW -4
STGW tCDown207.bestdist
!       COPY(temp, best)
CONST 80
GLOBAL tCDown207.best
CONST 80
GLOBAL tCDown207.temp
GLOBAL COPY
CALL 4
LABEL 35
RETURN
END

PROC tCDown207.Combine 12 6 0x00018001
! PROCEDURE Combine(r, s: INTEGER);
!   used := Bit.Or(r, s);
LDLW 16
LDLW 12
GLOBAL Bit.Or
CALLW 2
STLW -12
!   p := pool[r];
GLOBAL tCDown207.pool
LDLW 12
CONST 1024
BOUND 158
LDIW
STLW -4
LABEL 43
!   WHILE p # NIL DO
LDLW -4
JEQZ 45
!     q := pool[s];
GLOBAL tCDown207.pool
LDLW 16
CONST 1024
BOUND 160
LDIW
STLW -8
LABEL 46
!     WHILE q # NIL DO
LDLW -8
JEQZ 48
!       IF p.val >= q.val THEN
LDLW -4
NCHECK 162
LDNW 12
LDLW -8
NCHECK 162
LDNW 12
JLT 51
! 	Add(Plus, p, q, p.val+q.val, used);
LDLW -12
LDLW -4
NCHECK 163
LDNW 12
LDLW -8
NCHECK 163
LDNW 12
PLUS
LDLW -8
LDLW -4
CONST 1
GLOBAL tCDown207.Add
CALL 5
! 	IF p.val > q.val THEN Add(Minus, p, q, p.val-q.val, used) END;
LDLW -4
NCHECK 164
LDNW 12
LDLW -8
NCHECK 164
LDNW 12
JLEQ 54
LDLW -12
LDLW -4
NCHECK 164
LDNW 12
LDLW -8
NCHECK 164
LDNW 12
MINUS
LDLW -8
LDLW -4
CONST 2
GLOBAL tCDown207.Add
CALL 5
LABEL 54
! 	Add(Times, p, q, p.val*q.val, used);
LDLW -12
LDLW -4
NCHECK 165
LDNW 12
LDLW -8
NCHECK 165
LDNW 12
TIMES
LDLW -8
LDLW -4
CONST 3
GLOBAL tCDown207.Add
CALL 5
! 	IF (q.val > 0) & (p.val MOD q.val = 0) THEN
LDLW -8
NCHECK 166
LDNW 12
JLEQZ 51
LDLW -4
NCHECK 166
LDNW 12
LDLW -8
NCHECK 166
LDNW 12
ZCHECK 166
MOD
JNEQZ 51
! 	  Add(Divide, p, q, p.val DIV q.val, used)
LDLW -12
LDLW -4
NCHECK 167
LDNW 12
LDLW -8
NCHECK 167
LDNW 12
ZCHECK 167
DIV
LDLW -8
LDLW -4
CONST 4
GLOBAL tCDown207.Add
CALL 5
LABEL 51
!       q := q.next
LDLW -8
NCHECK 170
LDNW 20
STLW -8
JUMP 46
LABEL 48
!     p := p.next
LDLW -4
NCHECK 172
LDNW 20
STLW -4
JUMP 43
LABEL 45
RETURN
END

PROC tCDown207.Search 4140 6 0x00200001
! PROCEDURE Search(n: INTEGER; draw: ARRAY OF INTEGER);
!   ones[0] := 0; t := 1;
CONST 0
STLW -4112
CONST 1
STLW -16
!   FOR i := 0 TO n-1 DO
LDLW 12
DEC
STLW -4116
CONST 0
STLW -4
LABEL 59
LDLW -4
LDLW -4116
JGT 60
!     FOR r := 0 TO t-1 DO ones[t+r] := ones[r]+1 END;
LDLW -16
DEC
STLW -4120
CONST 0
STLW -8
LABEL 61
LDLW -8
LDLW -4120
JGT 62
LOCAL -4112
LDLW -8
CONST 1024
BOUND 196
LDIW
INC
LOCAL -4112
LDLW -16
LDLW -8
PLUS
CONST 1024
BOUND 196
STIW
INCL -8
JUMP 61
LABEL 62
!     t := 2*t
LDLW -16
CONST 2
TIMES
STLW -16
!   FOR i := 0 TO n-1 DO
INCL -4
JUMP 59
LABEL 60
!   FOR i := 0 TO HSIZE-1 DO htable[i] := NIL END;
CONST 0
STLW -4
LABEL 63
LDLW -4
CONST 19999
JGT 64
CONST 0
LDGW tCDown207.htable
NCHECK 201
LDLW -4
CONST 20000
BOUND 201
STIW
INCL -4
JUMP 63
LABEL 64
!   FOR r := 0 TO t-1 DO pool[r] := NIL END;
LDLW -16
DEC
STLW -4124
CONST 0
STLW -8
LABEL 65
LDLW -8
LDLW -4124
JGT 66
CONST 0
GLOBAL tCDown207.pool
LDLW -8
CONST 1024
BOUND 202
STIW
INCL -8
JUMP 65
LABEL 66
!   t := 1;
CONST 1
STLW -16
!   FOR i := 0 TO n-1 DO
LDLW 12
DEC
STLW -4128
CONST 0
STLW -4
LABEL 67
LDLW -4
LDLW -4128
JGT 68
!     Add(Const, NIL, NIL, draw[i], t);
LDLW -16
LDLW 16
LDLW -4
LDLW 20
BOUND 207
LDIW
CONST 0
CONST 0
CONST 0
GLOBAL tCDown207.Add
CALL 5
!     t := 2*t
LDLW -16
CONST 2
TIMES
STLW -16
!   FOR i := 0 TO n-1 DO
INCL -4
JUMP 67
LABEL 68
!   bestdist := 100000;
CONST 100000
STGW tCDown207.bestdist
!   FOR i := 2 TO n DO 
LDLW 12
STLW -4132
CONST 2
STLW -4
LABEL 69
LDLW -4
LDLW -4132
JGT 70
!     FOR r := 1 TO t-1 DO
LDLW -16
DEC
STLW -4136
CONST 1
STLW -8
LABEL 71
LDLW -8
LDLW -4136
JGT 72
!       FOR s := 1 TO t-1 DO
LDLW -16
DEC
STLW -4140
CONST 1
STLW -12
LABEL 73
LDLW -12
LDLW -4140
JGT 74
! 	IF (ones[r] + ones[s] = i) & (Bit.And(r, s) = 0) THEN
LOCAL -4112
LDLW -8
CONST 1024
BOUND 217
LDIW
LOCAL -4112
LDLW -12
CONST 1024
BOUND 217
LDIW
PLUS
LDLW -4
JNEQ 77
LDLW -12
LDLW -8
GLOBAL Bit.And
CALLW 2
JNEQZ 77
! 	  Combine(r, s)
LDLW -12
LDLW -8
GLOBAL tCDown207.Combine
CALL 2
LABEL 77
!       FOR s := 1 TO t-1 DO
INCL -12
JUMP 73
LABEL 74
!     FOR r := 1 TO t-1 DO
INCL -8
JUMP 71
LABEL 72
!   FOR i := 2 TO n DO 
INCL -4
JUMP 69
LABEL 70
RETURN
END

PROC tCDown207.Main 52 6 0
! PROCEDURE Main;
!   n := 6;
CONST 6
STLW -8
!   draw[0] := 100; draw[1] := 4; draw[2] := 25; 
CONST 100
STLW -48
CONST 4
STLW -44
CONST 25
STLW -40
!   draw[3] := 50; draw[4] := 2; draw[5] := 8; 
CONST 50
STLW -36
CONST 2
STLW -32
CONST 8
STLW -28
!   target := 999;
CONST 999
STGW tCDown207.target
!   Out.String("To make "); Out.Int(target, 0); Out.String(" from");
CONST 9
GLOBAL tCDown207.%4
GLOBAL Out.String
CALL 2
CONST 0
LDGW tCDown207.target
GLOBAL Out.Int
CALL 2
CONST 6
GLOBAL tCDown207.%5
GLOBAL Out.String
CALL 2
!   FOR i := 0 TO n-1 DO Out.Char(' '); Out.Int(draw[i], 0) END;
LDLW -8
DEC
STLW -52
CONST 0
STLW -4
LABEL 79
LDLW -4
LDLW -52
JGT 80
CONST 32
ALIGNC
GLOBAL Out.Char
CALL 1
CONST 0
LOCAL -48
LDLW -4
CONST 10
BOUND 281
LDIW
GLOBAL Out.Int
CALL 2
INCL -4
JUMP 79
LABEL 80
!   Out.Char(':'); Out.Ln;
CONST 58
ALIGNC
GLOBAL Out.Char
CALL 1
GLOBAL Out.Ln
CALL 0
!   Search(n, draw);
CONST 10
LOCAL -48
LDLW -8
GLOBAL tCDown207.Search
CALL 3
!   Out.Ln; Out.String("  "); Out.String(best); 
GLOBAL Out.Ln
CALL 0
CONST 3
GLOBAL tCDown207.%6
GLOBAL Out.String
CALL 2
CONST 80
GLOBAL tCDown207.best
GLOBAL Out.String
CALL 2
!   Out.String(" = "); Out.Int(bestval, 0);   
CONST 4
GLOBAL tCDown207.%7
GLOBAL Out.String
CALL 2
CONST 0
LDGW tCDown207.bestval
GLOBAL Out.Int
CALL 2
!   IF bestdist > 0 THEN 
LDGW tCDown207.bestdist
JLEQZ 83
!     Out.String(" (off by "); Out.Int(bestdist, 0); Out.Char(')')
CONST 10
GLOBAL tCDown207.%8
GLOBAL Out.String
CALL 2
CONST 0
LDGW tCDown207.bestdist
GLOBAL Out.Int
CALL 2
CONST 41
ALIGNC
GLOBAL Out.Char
CALL 1
LABEL 83
!   Out.Ln; Out.Ln
GLOBAL Out.Ln
CALL 0
GLOBAL Out.Ln
CALL 0
RETURN
END

PROC tCDown207.%main 0 6 0
!   GC.Debug("gs");
CONST 3
GLOBAL tCDown207.%9
GLOBAL GC.Debug
CALL 2
!   NEW(htable);
CONST 80000
GLOBAL tCDown207.%10
GLOBAL tCDown207.htable
GLOBAL NEW
CALL 3
!   Main;
GLOBAL tCDown207.Main
CALL 0
!   htable := NIL;
CONST 0
STGW tCDown207.htable
!   GC.Collect;
GLOBAL GC.Collect
CALL 0
!   Out.Ln
GLOBAL Out.Ln
CALL 0
RETURN
END

! Global variables
GLOVAR tCDown207.pool 4096
GLOVAR tCDown207.htable 4
GLOVAR tCDown207.target 4
GLOVAR tCDown207.temp 80
GLOVAR tCDown207.best 80
GLOVAR tCDown207.bestval 4
GLOVAR tCDown207.bestdist 4

! Pointer map
DEFINE tCDown207.%gcmap
WORD GC_BASE
WORD tCDown207.pool
WORD GC_BLOCK
WORD 0
WORD 1024
WORD GC_BASE
WORD tCDown207.htable
WORD 0
WORD GC_END

! String "?+-*/"
DEFINE tCDown207.%1
STRING 3F2B2D2A2F00

! String "01122"
DEFINE tCDown207.%2
STRING 303131323200

! String "01223"
DEFINE tCDown207.%3
STRING 303132323300

! String "To make "
DEFINE tCDown207.%4
STRING 546F206D616B652000

! String " from"
DEFINE tCDown207.%5
STRING 2066726F6D00

! String "  "
DEFINE tCDown207.%6
STRING 202000

! String " = "
DEFINE tCDown207.%7
STRING 203D2000

! String " (off by "
DEFINE tCDown207.%8
STRING 20286F66662062792000

! String "gs"
DEFINE tCDown207.%9
STRING 677300

! Descriptor for blob
DEFINE tCDown207.blob
WORD 0x000000cd
WORD 0
WORD tCDown207.blob.%anc

DEFINE tCDown207.blob.%anc
WORD tCDown207.blob

! Descriptor for *anon*
DEFINE tCDown207.%10
WORD tCDown207.%10.%map

! Pointer maps
DEFINE tCDown207.%10.%map
WORD GC_BLOCK
WORD 0
WORD 20000
WORD GC_END

! End of file
]]*)
