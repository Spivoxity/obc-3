MODULE tCDown1;

(*<<
To make 999 from 4 25 100 2 8 50:

  (50 - (100 + 4) / 8) * (25 + 2) = 999

>>*)

(* CountDown in Oberon *)

IMPORT Out, Conv, Strings;

CONST MAX = 20;			(* Maximum number of given numbers *)

CONST				(* Symbols for use in |exp| array *)
  Const = 0; Plus = 1; Minus = 2; Times = 3; Divide = 4;

VAR
  draw: ARRAY MAX+1 OF INTEGER;	(* Given numbers are draw[1..n+1) *)
  next: ARRAY MAX+1 OF INTEGER; (* Links for ring of unused numbers *)
  target: INTEGER;		(* Number to be made *)

  exp: ARRAY 2*MAX OF INTEGER;	(* |exp[0..i)| is an RPN string. *)
  val: ARRAY 2*MAX OF INTEGER;	(* Values of sub-expressions *)
  lnk: ARRAY 2*MAX OF INTEGER;	(* Links showing contents of stack *)


(* |Grind| converts an expression to printed form and returns it as a
   (statically allocated) string.  The output is simplified by
   omitting brackets where they are unnecessary because of the
   priority and associativity of operators.  Thus both of the
   expressions \verb|(1+2)+3| and \verb|1+(2+3)| will be shown without
   brackets. *)

CONST
  op = "?+-*/";			(* Character symbol for each operator *)
  pri = "01122";		(* Priorities of the operators *)
  rpri = "01223";		(* Priorities for right operands *)

TYPE buffer = ARRAY 80 OF CHAR;

PROCEDURE Grind(e0: INTEGER; VAR buf: buffer);
  VAR pos: INTEGER;			(* Current position in |buf| *)

  PROCEDURE Put(c: CHAR);
  BEGIN
    buf[pos] := c; pos := pos+1
  END Put;

  PROCEDURE Walk(e: INTEGER; p: INTEGER);
    VAR k, j, kp, rp: INTEGER; cbuf: ARRAY 10 OF CHAR;
  BEGIN
    k := exp[e];
    IF k = Const THEN
      (* A constant *)
      Conv.ConvInt(val[e], cbuf);
      j := 0;
      WHILE cbuf[j] # 0X DO Put(cbuf[j]); j := j+1 END
    ELSE
      (* A binary operator *)
      kp := ORD(pri[k]) - ORD('0');
      rp := ORD(rpri[k]) - ORD('0');
      IF kp < p THEN Put('(') END;
      Walk(lnk[e-1], kp);
      Put(' '); Put(op[k]); Put(' ');
      Walk(e-1, rp);
      IF kp < p THEN Put(')') END
    END
  END Walk;

BEGIN
  pos := 0;
  Walk(e0, 1);
  Put(0X)
END Grind;

VAR
  best: buffer;			(* Best found so far *)
  temp: buffer;			(* A new expression *)
  bestval, bestdist: INTEGER;	(* Value and distance from target *)

(* The search for a solution is done by the recursive function |Try|.
   On entry, |exp[0..i)| contains a valid postfix string, consisting of
   |d| expressions: thus |d| is the stack depth.  The function must
   complete this string in every possible way, recording the
   expression that gets nearest the target. *)

(* Try -- search for completion of postfix string *)
PROCEDURE Try(i: INTEGER; d: INTEGER);
  VAR p, q, a, b, dist: INTEGER;
BEGIN
  IF d = 1 THEN
    (* Stack depth 1: a complete expression *)
    dist := ABS(val[i-1] - target);
    IF dist <= bestdist THEN
      (* At least as close as before *)
      Grind(i-1, temp);
      IF (dist < bestdist) 
	  OR (Strings.Length(temp) < Strings.Length(best)) THEN
        (* Actually closer, or anyway shorter *)
	bestval := val[i-1];
	bestdist := dist;
	COPY(temp, best)
      END
    END
  END;

  (* Try adding a number from the ring of unused inputs *)
  p := 0; q := next[0];
  WHILE q # 0 DO
    (* If the same number appears several times, only use the
       first occurrence: this avoids generating the same
       expression in several ways.  We assume |draw[0] = 0|. *)
    IF draw[q] # draw[p] THEN
      exp[i] := Const;
      val[i] := draw[q];
      lnk[i] := i-1;

      (* Remove |q| from the ring during recursive call. *)
      next[p] := next[q];
      Try(i+1, d+1);
      next[p] := q;
    END;
    p := q; q := next[p];
  END;
	       
  IF d >= 2 THEN
    (* Two values on the stack: try using an operator *)
    a := val[lnk[i-1]]; b := val[i-1];

    (* We reject expressions like 3 - 5 or 3 / 5 that don't have
       a positive integer value: they are not allowed in game
       show arithmetic.  To cut down the search space, we also
       reject expressions a + b and a * b where a < b: an
       equivalent expression will be tried at some other
       time. Also a * 1 and 1 * b, which never give the shortest
       expression.

       In addition, we can forbid expressions like 5 + (4 + 3),
       preferring (5 + 4) + 3.  Also, we can prefer (5 + 4) - 3
       to (5 - 3) + 4.  Again, we can forbid (5 + 3) + 4 in
       favour of (5 + 4) + 3, so that all operands to an
       associative and commutative operator appear in descending
       order. This further cuts down the branching. *)

    lnk[i] := lnk[lnk[i-1]];

    IF (a >= b) & (exp[i-1] # Plus) & (exp[i-1] # Minus)
        & (exp[lnk[i-1]] # Minus)
	& ((exp[lnk[i-1]] # Plus) OR (val[lnk[i-1]-1] >= val[i-1])) THEN
      exp[i] := Plus;
      val[i] := a + b;
      Try(i+1, d-1)
    END;

    IF (a > b) & (exp[i-1] # Plus) & (exp[i-1] # Minus)
        & ((exp[lnk[i-1]] # Minus) OR (val[lnk[i-1]-1] >= val[i-1])) THEN
      exp[i] := Minus;
      val[i] := a - b; 
      Try(i+1, d-1)
    END;

    IF (a > 1) & (b > 1) & (a >= b)
       & (exp[i-1] # Times) & (exp[i-1] # Divide)
       & (exp[lnk[i-1]] # Divide)
       & ((exp[lnk[i-1]] # Times) OR (val[lnk[i-1]-1] >= val[i-1])) THEN
      exp[i] := Times;
      val[i] := a * b;
      Try(i+1, d-1)
    END;

    (* For some reason, 6 / (3 * 2) seems better than 6 / 3 / 2. *)
    IF (b > 1) & (a MOD b = 0) & (exp[i-1] # Divide)
        & (exp[lnk[i-1]] # Divide) THEN
      exp[i] := Divide;
      val[i] := a DIV b;
      Try(i+1, d-1)
    END
  END
END Try;


(* Search -- search for all ways to reach the target and print the best *)
PROCEDURE Search(n: INTEGER);
  VAR i, j, t: INTEGER;
BEGIN
  (* Print numbers in original order (ascending if we chose them). *)
  Out.String("To make "); Out.Int(target, 0); Out.String(" from");
  FOR i := 1 TO n DO Out.Char(' '); Out.Int(draw[i], 0) END;
  Out.Char(':'); Out.Ln;

  (* Sort the input numbers to help with detection of duplicates.
     Decreasing order is marginally better as it avoids dead ends
     early in the search.  We use insertion sort. *)
  FOR i := 2 TO n DO
    t := draw[i]; j := i;
    WHILE (j > 1) & (t > draw[j-1]) DO
      draw[j] := draw[j-1]; j := j-1
    END;
    draw[j] := t
  END;
	  
  (* Link the numbers into a ring with next[0] as a list header. *)
  FOR i := 0 TO n-1 DO next[i] := i+1 END;
  next[n] := 0;

  (* Find the best answer. *)
  draw[0] := 0;
  bestdist := 100000;
  Try(0, 0);

  (* Print it. *)
  Out.Ln; Out.String("  "); Out.String(best); 
  Out.String(" = "); Out.Int(bestval, 0);   
  IF bestdist > 0 THEN 
    Out.String(" (off by "); Out.Int(bestdist, 0); Out.Char(')')
  END;
  Out.Ln; Out.Ln
END Search;

(*
PROCEDURE RandChoice;
  VAR i, j: INTEGER;
BEGIN
  (* We want 5 numbers from 1, 1, 2, 2, ..., 10, 10, with each
     number equally likely to be chosen. The laws of
     conditional probablility show how to do it: at each stage,
     we must select |(5-j)| numbers from the |(20-i)| that remain. *)
  FOR i := 0 TO 19 DO
    (* Choose |i/2+1| with probability |(5-j)/(20-i)| *)
    IF Random.Roll(20-i) < 5-j THEN
      draw[j+1] := i DIV 2 + 1;
      j := j+1
    END
  END;

  draw[6] := 25 * (Random.Roll(4) + 1);
  target := 101 + Random.Roll(899)
END RandChoice;
*)

PROCEDURE Main;
  VAR (* i, *) n: INTEGER; (* buf: ARRAY 20 OF CHAR; *)
BEGIN
  n := 6;
  draw[1] := 4; draw[2] := 25; draw[3] := 100;
  draw[4] := 2; draw[5] := 8; draw[6] := 50;
  target := 999;

(*
  IF Args.argc = 1 THEN
    (* Random problem *)
    n := 6;
    Random.Randomize;
    RandChoice;
  ELSIF (Args.argc >= 3) & (Args.argc < MAX+1) THEN
    (* Problem specified on command line *)
    n := Args.argc-2;
    FOR i := 1 TO n DO
      Args.GetArg(i, buf);
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

  Search(n);
END Main;

BEGIN
  Main
END tCDown1.

(*[[
!! SYMFILE #tCDown1 STAMP #tCDown1.%main 1
!! END STAMP
!! 
MODULE tCDown1 STAMP 0
IMPORT Out STAMP
IMPORT Conv STAMP
IMPORT Strings STAMP
ENDHDR

PROC tCDown1.%9.Put 1 4 0
!   PROCEDURE Put(c: CHAR);
SAVELINK
!     buf[pos] := c; pos := pos+1
LDLC 12
LDEW 16
LDEW -4
CONST 80
BOUND 48
STIC
LDEW -4
INC
STEW -4
RETURN
END

PROC tCDown1.%10.Walk 8 5 0
!   PROCEDURE Walk(e: INTEGER; p: INTEGER);
SAVELINK
!     k := exp[e];
GLOBAL tCDown1.exp
LDLW 12
CONST 40
BOUND 54
LDIW
STLW -8
!     IF k = Const THEN
LDLW -8
JNEQZ 12
!       Conv.ConvInt(val[e], cbuf);
CONST 10
LOCAL -30
GLOBAL tCDown1.val
LDLW 12
CONST 40
BOUND 57
LDIW
GLOBAL Conv.ConvInt
CALL 3
!       j := 0;
CONST 0
STLW -12
JUMP 14
LABEL 13
LOCAL -30
LDLW -12
CONST 10
BOUND 59
LDIC
ALIGNC
LDLW -4
LINK
GLOBAL tCDown1.%9.Put
CALL 1
INCL -12
LABEL 14
!       WHILE cbuf[j] # 0X DO Put(cbuf[j]); j := j+1 END
LOCAL -30
LDLW -12
CONST 10
BOUND 59
LDIC
JNEQZ 13
RETURN
LABEL 12
!       kp := ORD(pri[k]) - ORD('0');
GLOBAL tCDown1.%2
LDLW -8
CONST 6
BOUND 62
LDIC
CONST 48
MINUS
STLW -16
!       rp := ORD(rpri[k]) - ORD('0');
GLOBAL tCDown1.%3
LDLW -8
CONST 6
BOUND 63
LDIC
CONST 48
MINUS
STLW -20
!       IF kp < p THEN Put('(') END;
LDLW -16
LDLW 16
JGEQ 16
CONST 40
ALIGNC
LDLW -4
LINK
GLOBAL tCDown1.%9.Put
CALL 1
LABEL 16
!       Walk(lnk[e-1], kp);
LDLW -16
GLOBAL tCDown1.lnk
LDLW 12
DEC
CONST 40
BOUND 65
LDIW
LDLW -4
LINK
GLOBAL tCDown1.%10.Walk
CALL 2
!       Put(' '); Put(op[k]); Put(' ');
CONST 32
ALIGNC
LDLW -4
LINK
GLOBAL tCDown1.%9.Put
CALL 1
GLOBAL tCDown1.%1
LDLW -8
CONST 6
BOUND 66
LDIC
ALIGNC
LDLW -4
LINK
GLOBAL tCDown1.%9.Put
CALL 1
CONST 32
ALIGNC
LDLW -4
LINK
GLOBAL tCDown1.%9.Put
CALL 1
!       Walk(e-1, rp);
LDLW -20
LDLW 12
DEC
LDLW -4
LINK
GLOBAL tCDown1.%10.Walk
CALL 2
!       IF kp < p THEN Put(')') END
LDLW -16
LDLW 16
JGEQ 18
CONST 41
ALIGNC
LDLW -4
LINK
GLOBAL tCDown1.%9.Put
CALL 1
LABEL 18
RETURN
END

PROC tCDown1.Grind 1 5 0x00200001
! PROCEDURE Grind(e0: INTEGER; VAR buf: buffer);
!   pos := 0;
CONST 0
STLW -4
!   Walk(e0, 1);
CONST 1
LDLW 12
LOCAL 0
LINK
GLOBAL tCDown1.%10.Walk
CALL 2
!   Put(0X)
CONST 0
ALIGNC
LOCAL 0
LINK
GLOBAL tCDown1.%9.Put
CALL 1
RETURN
END

PROC tCDown1.Try 5 5 0
! PROCEDURE Try(i: INTEGER; d: INTEGER);
!   IF d = 1 THEN
LDLW 16
CONST 1
JNEQ 20
!     dist := ABS(val[i-1] - target);
GLOBAL tCDown1.val
LDLW 12
DEC
CONST 40
BOUND 95
LDIW
LDGW tCDown1.target
MINUS
GLOBAL ABSINT
CALLW 1
STLW -20
!     IF dist <= bestdist THEN
LDLW -20
LDGW tCDown1.bestdist
JGT 20
!       Grind(i-1, temp);
GLOBAL tCDown1.temp
LDLW 12
DEC
GLOBAL tCDown1.Grind
CALL 2
!       IF (dist < bestdist) 
LDLW -20
LDGW tCDown1.bestdist
JLT 25
! 	  OR (Strings.Length(temp) < Strings.Length(best)) THEN
CONST 80
GLOBAL tCDown1.temp
GLOBAL Strings.Length
CALLW 2
CONST 80
GLOBAL tCDown1.best
GLOBAL Strings.Length
CALLW 2
JGEQ 20
LABEL 25
! 	bestval := val[i-1];
GLOBAL tCDown1.val
LDLW 12
DEC
CONST 40
BOUND 102
LDIW
STGW tCDown1.bestval
! 	bestdist := dist;
LDLW -20
STGW tCDown1.bestdist
! 	COPY(temp, best)
CONST 80
GLOBAL tCDown1.best
CONST 80
GLOBAL tCDown1.temp
GLOBAL COPY
CALL 4
LABEL 20
!   p := 0; q := next[0];
CONST 0
STLW -4
LDGW tCDown1.next
STLW -8
JUMP 27
LABEL 26
!     IF draw[q] # draw[p] THEN
GLOBAL tCDown1.draw
LDLW -8
CONST 21
BOUND 115
LDIW
GLOBAL tCDown1.draw
LDLW -4
CONST 21
BOUND 115
LDIW
JEQ 29
!       exp[i] := Const;
CONST 0
GLOBAL tCDown1.exp
LDLW 12
CONST 40
BOUND 116
STIW
!       val[i] := draw[q];
GLOBAL tCDown1.draw
LDLW -8
CONST 21
BOUND 117
LDIW
GLOBAL tCDown1.val
LDLW 12
CONST 40
BOUND 117
STIW
!       lnk[i] := i-1;
LDLW 12
DEC
GLOBAL tCDown1.lnk
LDLW 12
CONST 40
BOUND 118
STIW
!       next[p] := next[q];
GLOBAL tCDown1.next
LDLW -8
CONST 21
BOUND 121
LDIW
GLOBAL tCDown1.next
LDLW -4
CONST 21
BOUND 121
STIW
!       Try(i+1, d+1);
LDLW 16
INC
LDLW 12
INC
GLOBAL tCDown1.Try
CALL 2
!       next[p] := q;
LDLW -8
GLOBAL tCDown1.next
LDLW -4
CONST 21
BOUND 123
STIW
LABEL 29
!     p := q; q := next[p];
LDLW -8
STLW -4
GLOBAL tCDown1.next
LDLW -4
CONST 21
BOUND 125
LDIW
STLW -8
LABEL 27
!   WHILE q # 0 DO
LDLW -8
JNEQZ 26
!   IF d >= 2 THEN
LDLW 16
CONST 2
JLT 31
!     a := val[lnk[i-1]]; b := val[i-1];
GLOBAL tCDown1.val
GLOBAL tCDown1.lnk
LDLW 12
DEC
CONST 40
BOUND 130
LDIW
CONST 40
BOUND 130
LDIW
STLW -12
GLOBAL tCDown1.val
LDLW 12
DEC
CONST 40
BOUND 130
LDIW
STLW -16
!     lnk[i] := lnk[lnk[i-1]];
GLOBAL tCDown1.lnk
GLOBAL tCDown1.lnk
LDLW 12
DEC
CONST 40
BOUND 147
LDIW
CONST 40
BOUND 147
LDIW
GLOBAL tCDown1.lnk
LDLW 12
CONST 40
BOUND 147
STIW
!     IF (a >= b) & (exp[i-1] # Plus) & (exp[i-1] # Minus)
LDLW -12
LDLW -16
JLT 33
GLOBAL tCDown1.exp
LDLW 12
DEC
CONST 40
BOUND 149
LDIW
CONST 1
JEQ 33
GLOBAL tCDown1.exp
LDLW 12
DEC
CONST 40
BOUND 149
LDIW
CONST 2
JEQ 33
!         & (exp[lnk[i-1]] # Minus)
GLOBAL tCDown1.exp
GLOBAL tCDown1.lnk
LDLW 12
DEC
CONST 40
BOUND 150
LDIW
CONST 40
BOUND 150
LDIW
CONST 2
JEQ 33
! 	& ((exp[lnk[i-1]] # Plus) OR (val[lnk[i-1]-1] >= val[i-1])) THEN
GLOBAL tCDown1.exp
GLOBAL tCDown1.lnk
LDLW 12
DEC
CONST 40
BOUND 151
LDIW
CONST 40
BOUND 151
LDIW
CONST 1
JNEQ 34
GLOBAL tCDown1.val
GLOBAL tCDown1.lnk
LDLW 12
DEC
CONST 40
BOUND 151
LDIW
DEC
CONST 40
BOUND 151
LDIW
GLOBAL tCDown1.val
LDLW 12
DEC
CONST 40
BOUND 151
LDIW
JLT 33
LABEL 34
!       exp[i] := Plus;
CONST 1
GLOBAL tCDown1.exp
LDLW 12
CONST 40
BOUND 152
STIW
!       val[i] := a + b;
LDLW -12
LDLW -16
PLUS
GLOBAL tCDown1.val
LDLW 12
CONST 40
BOUND 153
STIW
!       Try(i+1, d-1)
LDLW 16
DEC
LDLW 12
INC
GLOBAL tCDown1.Try
CALL 2
LABEL 33
!     IF (a > b) & (exp[i-1] # Plus) & (exp[i-1] # Minus)
LDLW -12
LDLW -16
JLEQ 36
GLOBAL tCDown1.exp
LDLW 12
DEC
CONST 40
BOUND 157
LDIW
CONST 1
JEQ 36
GLOBAL tCDown1.exp
LDLW 12
DEC
CONST 40
BOUND 157
LDIW
CONST 2
JEQ 36
!         & ((exp[lnk[i-1]] # Minus) OR (val[lnk[i-1]-1] >= val[i-1])) THEN
GLOBAL tCDown1.exp
GLOBAL tCDown1.lnk
LDLW 12
DEC
CONST 40
BOUND 158
LDIW
CONST 40
BOUND 158
LDIW
CONST 2
JNEQ 37
GLOBAL tCDown1.val
GLOBAL tCDown1.lnk
LDLW 12
DEC
CONST 40
BOUND 158
LDIW
DEC
CONST 40
BOUND 158
LDIW
GLOBAL tCDown1.val
LDLW 12
DEC
CONST 40
BOUND 158
LDIW
JLT 36
LABEL 37
!       exp[i] := Minus;
CONST 2
GLOBAL tCDown1.exp
LDLW 12
CONST 40
BOUND 159
STIW
!       val[i] := a - b; 
LDLW -12
LDLW -16
MINUS
GLOBAL tCDown1.val
LDLW 12
CONST 40
BOUND 160
STIW
!       Try(i+1, d-1)
LDLW 16
DEC
LDLW 12
INC
GLOBAL tCDown1.Try
CALL 2
LABEL 36
!     IF (a > 1) & (b > 1) & (a >= b)
LDLW -12
CONST 1
JLEQ 39
LDLW -16
CONST 1
JLEQ 39
LDLW -12
LDLW -16
JLT 39
!        & (exp[i-1] # Times) & (exp[i-1] # Divide)
GLOBAL tCDown1.exp
LDLW 12
DEC
CONST 40
BOUND 165
LDIW
CONST 3
JEQ 39
GLOBAL tCDown1.exp
LDLW 12
DEC
CONST 40
BOUND 165
LDIW
CONST 4
JEQ 39
!        & (exp[lnk[i-1]] # Divide)
GLOBAL tCDown1.exp
GLOBAL tCDown1.lnk
LDLW 12
DEC
CONST 40
BOUND 166
LDIW
CONST 40
BOUND 166
LDIW
CONST 4
JEQ 39
!        & ((exp[lnk[i-1]] # Times) OR (val[lnk[i-1]-1] >= val[i-1])) THEN
GLOBAL tCDown1.exp
GLOBAL tCDown1.lnk
LDLW 12
DEC
CONST 40
BOUND 167
LDIW
CONST 40
BOUND 167
LDIW
CONST 3
JNEQ 40
GLOBAL tCDown1.val
GLOBAL tCDown1.lnk
LDLW 12
DEC
CONST 40
BOUND 167
LDIW
DEC
CONST 40
BOUND 167
LDIW
GLOBAL tCDown1.val
LDLW 12
DEC
CONST 40
BOUND 167
LDIW
JLT 39
LABEL 40
!       exp[i] := Times;
CONST 3
GLOBAL tCDown1.exp
LDLW 12
CONST 40
BOUND 168
STIW
!       val[i] := a * b;
LDLW -12
LDLW -16
TIMES
GLOBAL tCDown1.val
LDLW 12
CONST 40
BOUND 169
STIW
!       Try(i+1, d-1)
LDLW 16
DEC
LDLW 12
INC
GLOBAL tCDown1.Try
CALL 2
LABEL 39
!     IF (b > 1) & (a MOD b = 0) & (exp[i-1] # Divide)
LDLW -16
CONST 1
JLEQ 31
LDLW -12
LDLW -16
ZCHECK 174
MOD
JNEQZ 31
GLOBAL tCDown1.exp
LDLW 12
DEC
CONST 40
BOUND 174
LDIW
CONST 4
JEQ 31
!         & (exp[lnk[i-1]] # Divide) THEN
GLOBAL tCDown1.exp
GLOBAL tCDown1.lnk
LDLW 12
DEC
CONST 40
BOUND 175
LDIW
CONST 40
BOUND 175
LDIW
CONST 4
JEQ 31
!       exp[i] := Divide;
CONST 4
GLOBAL tCDown1.exp
LDLW 12
CONST 40
BOUND 176
STIW
!       val[i] := a DIV b;
LDLW -12
LDLW -16
ZCHECK 177
DIV
GLOBAL tCDown1.val
LDLW 12
CONST 40
BOUND 177
STIW
!       Try(i+1, d-1)
LDLW 16
DEC
LDLW 12
INC
GLOBAL tCDown1.Try
CALL 2
LABEL 31
RETURN
END

PROC tCDown1.Search 6 5 0
! PROCEDURE Search(n: INTEGER);
!   Out.String("To make "); Out.Int(target, 0); Out.String(" from");
CONST 9
GLOBAL tCDown1.%4
GLOBAL Out.String
CALL 2
CONST 0
LDGW tCDown1.target
GLOBAL Out.Int
CALL 2
CONST 6
GLOBAL tCDown1.%5
GLOBAL Out.String
CALL 2
!   FOR i := 1 TO n DO Out.Char(' '); Out.Int(draw[i], 0) END;
LDLW 12
STLW -16
CONST 1
STLW -4
JUMP 44
LABEL 43
CONST 32
ALIGNC
GLOBAL Out.Char
CALL 1
CONST 0
GLOBAL tCDown1.draw
LDLW -4
CONST 21
BOUND 190
LDIW
GLOBAL Out.Int
CALL 2
INCL -4
LABEL 44
LDLW -4
LDLW -16
JLEQ 43
!   Out.Char(':'); Out.Ln;
CONST 58
ALIGNC
GLOBAL Out.Char
CALL 1
GLOBAL Out.Ln
CALL 0
!   FOR i := 2 TO n DO
LDLW 12
STLW -20
CONST 2
STLW -4
JUMP 46
LABEL 45
!     t := draw[i]; j := i;
GLOBAL tCDown1.draw
LDLW -4
CONST 21
BOUND 197
LDIW
STLW -12
LDLW -4
STLW -8
JUMP 48
LABEL 47
!       draw[j] := draw[j-1]; j := j-1
GLOBAL tCDown1.draw
LDLW -8
DEC
CONST 21
BOUND 199
LDIW
GLOBAL tCDown1.draw
LDLW -8
CONST 21
BOUND 199
STIW
DECL -8
LABEL 48
!     WHILE (j > 1) & (t > draw[j-1]) DO
LDLW -8
CONST 1
JLEQ 49
LDLW -12
GLOBAL tCDown1.draw
LDLW -8
DEC
CONST 21
BOUND 198
LDIW
JGT 47
LABEL 49
!     draw[j] := t
LDLW -12
GLOBAL tCDown1.draw
LDLW -8
CONST 21
BOUND 201
STIW
!   FOR i := 2 TO n DO
INCL -4
LABEL 46
LDLW -4
LDLW -20
JLEQ 45
!   FOR i := 0 TO n-1 DO next[i] := i+1 END;
LDLW 12
DEC
STLW -24
CONST 0
STLW -4
JUMP 51
LABEL 50
LDLW -4
INC
GLOBAL tCDown1.next
LDLW -4
CONST 21
BOUND 205
STIW
INCL -4
LABEL 51
LDLW -4
LDLW -24
JLEQ 50
!   next[n] := 0;
CONST 0
GLOBAL tCDown1.next
LDLW 12
CONST 21
BOUND 206
STIW
!   draw[0] := 0;
CONST 0
STGW tCDown1.draw
!   bestdist := 100000;
CONST 100000
STGW tCDown1.bestdist
!   Try(0, 0);
CONST 0
CONST 0
GLOBAL tCDown1.Try
CALL 2
!   Out.Ln; Out.String("  "); Out.String(best); 
GLOBAL Out.Ln
CALL 0
CONST 3
GLOBAL tCDown1.%6
GLOBAL Out.String
CALL 2
CONST 80
GLOBAL tCDown1.best
GLOBAL Out.String
CALL 2
!   Out.String(" = "); Out.Int(bestval, 0);   
CONST 4
GLOBAL tCDown1.%7
GLOBAL Out.String
CALL 2
CONST 0
LDGW tCDown1.bestval
GLOBAL Out.Int
CALL 2
!   IF bestdist > 0 THEN 
LDGW tCDown1.bestdist
JLEQZ 53
!     Out.String(" (off by "); Out.Int(bestdist, 0); Out.Char(')')
CONST 10
GLOBAL tCDown1.%8
GLOBAL Out.String
CALL 2
CONST 0
LDGW tCDown1.bestdist
GLOBAL Out.Int
CALL 2
CONST 41
ALIGNC
GLOBAL Out.Char
CALL 1
LABEL 53
!   Out.Ln; Out.Ln
GLOBAL Out.Ln
CALL 0
GLOBAL Out.Ln
CALL 0
RETURN
END

PROC tCDown1.Main 1 5 0
! PROCEDURE Main;
!   n := 6;
CONST 6
STLW -4
!   draw[1] := 4; draw[2] := 25; draw[3] := 100;
CONST 4
GLOBAL tCDown1.draw
STNW 4
CONST 25
GLOBAL tCDown1.draw
STNW 8
CONST 100
GLOBAL tCDown1.draw
STNW 12
!   draw[4] := 2; draw[5] := 8; draw[6] := 50;
CONST 2
GLOBAL tCDown1.draw
STNW 16
CONST 8
GLOBAL tCDown1.draw
STNW 20
CONST 50
GLOBAL tCDown1.draw
STNW 24
!   target := 999;
CONST 999
STGW tCDown1.target
!   Search(n);
LDLW -4
GLOBAL tCDown1.Search
CALL 1
RETURN
END

PROC tCDown1.%main 0 5 0
!   Main
GLOBAL tCDown1.Main
CALL 0
RETURN
END

! Global variables
GLOVAR tCDown1.draw 84
GLOVAR tCDown1.next 84
GLOVAR tCDown1.target 4
GLOVAR tCDown1.exp 160
GLOVAR tCDown1.val 160
GLOVAR tCDown1.lnk 160
GLOVAR tCDown1.best 80
GLOVAR tCDown1.temp 80
GLOVAR tCDown1.bestval 4
GLOVAR tCDown1.bestdist 4

! String "?+-*/"
DEFINE tCDown1.%1
STRING 3F2B2D2A2F00

! String "01122"
DEFINE tCDown1.%2
STRING 303131323200

! String "01223"
DEFINE tCDown1.%3
STRING 303132323300

! String "To make "
DEFINE tCDown1.%4
STRING 546F206D616B652000

! String " from"
DEFINE tCDown1.%5
STRING 2066726F6D00

! String "  "
DEFINE tCDown1.%6
STRING 202000

! String " = "
DEFINE tCDown1.%7
STRING 203D2000

! String " (off by "
DEFINE tCDown1.%8
STRING 20286F66662062792000

! End of file
]]*)