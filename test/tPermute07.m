MODULE tPermute07;

IMPORT Random, Out;

CONST N = 5;

VAR val: ARRAY 2*N+1 OF INTEGER;

PROCEDURE Move(i, j: INTEGER);
BEGIN
  Out.String(" "); Out.Int(i, 0); Out.String("<-"); Out.Int(j, 0);
  val[i] := val[j]
END Move;

PROCEDURE Permute(VAR arg: ARRAY OF INTEGER);
  VAR 
    i, j, k: INTEGER; 
    changed: BOOLEAN; 
    u: ARRAY N OF INTEGER;
BEGIN
  (* Set u[i] to the number of uses of register i *)
  FOR i := 0 TO N-1 DO u[i] := 0 END;
  FOR i := 0 TO N-1 DO
    IF arg[i] < N THEN INC(u[arg[i]]) END
  END;

  (* First deal with acyclic parts of the use graph *)
  REPEAT
    changed := FALSE;
    FOR i := 0 TO N-1 DO
      IF (arg[i] # i) & (u[i] = 0) THEN
        Move(i, arg[i]);
        IF arg[i] < N THEN DEC(u[arg[i]]) END;
        arg[i] := i; changed := TRUE
      END
    END
  UNTIL ~changed;

  (* What remains is made up of cycles. *)
  FOR i := 0 TO N-1 DO
    IF arg[i] # i THEN
      Move(2*N, i);
      j := i;
      WHILE arg[j] # i DO
        Move(j, arg[j]);
        k := arg[j]; arg[j] := j; j := k
      END;
      Move(j, 2*N);
      arg[j] := j
    END
  END
END Permute;

PROCEDURE Test;
  VAR 
    i: INTEGER;
    arg, expected: ARRAY N OF INTEGER;
BEGIN
  FOR i := 0 TO 2*N DO val[i] := Random.Random() END;

  Out.String("Args ");
  FOR i := 0 TO N-1 DO
    arg[i] := Random.Roll(2*N);
    expected[i] := val[arg[i]];
    Out.String(" "); Out.Int(arg[i], 0)
  END;
  Out.Ln;

  Out.String("Move ");
  Permute(arg);
  Out.Ln;

  FOR i := 0 TO N-1 DO
    IF val[i] # expected[i] THEN
      Out.String("***Failed***"); Out.Ln;
    END
  END;

  Out.Ln
END Test;

PROCEDURE Main;
  VAR n: INTEGER;
BEGIN
  FOR n := 0 TO 99 DO Test END
END Main;

BEGIN
  Main
END tPermute07.

(*<<
Args  5 9 0 7 6
Move  1<-9 2<-0 3<-7 4<-6 0<-5

Args  7 7 7 5 3
Move  0<-7 1<-7 2<-7 4<-3 3<-5

Args  8 3 3 1 7
Move  0<-8 2<-3 4<-7 10<-1 1<-3 3<-10

Args  0 6 2 0 1
Move  3<-0 4<-1 1<-6

Args  4 9 0 2 9
Move  1<-9 3<-2 2<-0 0<-4 4<-9

Args  7 8 6 4 2
Move  0<-7 1<-8 3<-4 4<-2 2<-6

Args  8 8 3 2 7
Move  0<-8 1<-8 4<-7 10<-2 2<-3 3<-10

Args  1 2 1 5 3
Move  0<-1 4<-3 3<-5 10<-1 1<-2 2<-10

Args  1 4 6 0 7
Move  2<-6 3<-0 0<-1 1<-4 4<-7

Args  6 3 0 4 9
Move  1<-3 2<-0 3<-4 4<-9 0<-6

Args  4 6 2 7 3
Move  0<-4 1<-6 4<-3 3<-7

Args  4 0 4 2 7
Move  1<-0 3<-2 0<-4 2<-4 4<-7

Args  3 4 5 5 5
Move  0<-3 1<-4 2<-5 3<-5 4<-5

Args  8 5 9 3 8
Move  0<-8 1<-5 2<-9 4<-8

Args  2 4 1 7 9
Move  0<-2 2<-1 3<-7 1<-4 4<-9

Args  7 1 8 5 3
Move  0<-7 2<-8 4<-3 3<-5

Args  3 7 9 5 9
Move  0<-3 1<-7 2<-9 3<-5 4<-9

Args  0 7 3 4 7
Move  1<-7 2<-3 3<-4 4<-7

Args  8 6 2 7 4
Move  0<-8 1<-6 3<-7

Args  7 1 7 6 9
Move  0<-7 2<-7 3<-6 4<-9

Args  2 8 4 7 0
Move  1<-8 3<-7 10<-0 0<-2 2<-4 4<-10

Args  6 8 0 7 3
Move  1<-8 2<-0 4<-3 0<-6 3<-7

Args  4 1 0 3 8
Move  2<-0 0<-4 4<-8

Args  5 8 3 4 1
Move  0<-5 2<-3 3<-4 4<-1 1<-8

Args  5 6 0 5 3
Move  1<-6 2<-0 4<-3 0<-5 3<-5

Args  1 2 0 3 6
Move  4<-6 10<-0 0<-1 1<-2 2<-10

Args  6 5 2 0 4
Move  1<-5 3<-0 0<-6

Args  9 2 9 4 1
Move  0<-9 3<-4 4<-1 1<-2 2<-9

Args  4 1 8 7 0
Move  2<-8 3<-7 10<-0 0<-4 4<-10

Args  2 6 0 4 2
Move  1<-6 3<-4 4<-2 10<-0 0<-2 2<-10

Args  7 8 6 8 9
Move  0<-7 1<-8 2<-6 3<-8 4<-9

Args  9 4 6 3 1
Move  0<-9 2<-6 10<-1 1<-4 4<-10

Args  1 0 9 8 0
Move  2<-9 3<-8 4<-0 10<-0 0<-1 1<-10

Args  5 9 8 1 5
Move  0<-5 2<-8 3<-1 4<-5 1<-9

Args  1 5 0 6 6
Move  2<-0 3<-6 4<-6 0<-1 1<-5

Args  7 8 1 7 0
Move  2<-1 3<-7 4<-0 0<-7 1<-8

Args  8 6 6 9 3
Move  0<-8 1<-6 2<-6 4<-3 3<-9

Args  1 2 9 0 8
Move  3<-0 4<-8 0<-1 1<-2 2<-9

Args  4 5 9 2 5
Move  0<-4 1<-5 3<-2 4<-5 2<-9

Args  0 0 2 3 6
Move  1<-0 4<-6

Args  8 7 5 0 0
Move  1<-7 2<-5 3<-0 4<-0 0<-8

Args  3 6 2 3 1
Move  0<-3 4<-1 1<-6

Args  4 9 7 4 5
Move  0<-4 1<-9 2<-7 3<-4 4<-5

Args  4 9 9 4 9
Move  0<-4 1<-9 2<-9 3<-4 4<-9

Args  7 0 0 5 0
Move  1<-0 2<-0 3<-5 4<-0 0<-7

Args  8 1 3 4 6
Move  0<-8 2<-3 3<-4 4<-6

Args  3 9 8 1 7
Move  0<-3 2<-8 3<-1 4<-7 1<-9

Args  6 2 4 0 9
Move  1<-2 2<-4 3<-0 4<-9 0<-6

Args  1 2 8 4 6
Move  0<-1 1<-2 2<-8 3<-4 4<-6

Args  2 6 2 5 7
Move  0<-2 1<-6 3<-5 4<-7

Args  1 8 1 6 0
Move  2<-1 3<-6 4<-0 0<-1 1<-8

Args  7 9 3 8 8
Move  0<-7 1<-9 2<-3 3<-8 4<-8

Args  9 0 3 0 5
Move  1<-0 2<-3 3<-0 4<-5 0<-9

Args  4 8 6 6 2
Move  0<-4 1<-8 3<-6 4<-2 2<-6

Args  7 3 8 2 5
Move  0<-7 1<-3 3<-2 4<-5 2<-8

Args  2 2 5 8 8
Move  0<-2 1<-2 2<-5 3<-8 4<-8

Args  2 9 2 5 8
Move  0<-2 1<-9 3<-5 4<-8

Args  7 4 3 6 6
Move  0<-7 1<-4 2<-3 3<-6 4<-6

Args  9 4 4 2 4
Move  0<-9 1<-4 3<-2 2<-4

Args  0 1 1 7 3
Move  2<-1 4<-3 3<-7

Args  9 6 2 5 2
Move  0<-9 1<-6 3<-5 4<-2

Args  3 5 7 5 5
Move  0<-3 1<-5 2<-7 3<-5 4<-5

Args  9 0 4 4 5
Move  1<-0 2<-4 3<-4 4<-5 0<-9

Args  5 2 2 1 1
Move  0<-5 3<-1 4<-1 1<-2

Args  7 0 3 3 9
Move  1<-0 2<-3 4<-9 0<-7

Args  9 0 6 8 4
Move  1<-0 2<-6 3<-8 0<-9

Args  1 1 3 7 3
Move  0<-1 2<-3 4<-3 3<-7

Args  3 7 4 4 0
Move  1<-7 2<-4 10<-0 0<-3 3<-4 4<-10

Args  0 1 4 6 4
Move  2<-4 3<-6

Args  7 1 5 4 6
Move  0<-7 2<-5 3<-4 4<-6

Args  4 2 8 6 3
Move  0<-4 1<-2 2<-8 4<-3 3<-6

Args  5 6 8 1 6
Move  0<-5 2<-8 3<-1 4<-6 1<-6

Args  1 4 3 7 7
Move  0<-1 1<-4 2<-3 3<-7 4<-7

Args  9 5 2 5 5
Move  0<-9 1<-5 3<-5 4<-5

Args  4 8 9 6 2
Move  0<-4 1<-8 3<-6 4<-2 2<-9

Args  4 0 9 7 7
Move  1<-0 2<-9 3<-7 0<-4 4<-7

Args  7 8 4 2 2
Move  0<-7 1<-8 3<-2 10<-2 2<-4 4<-10

Args  2 3 8 5 5
Move  0<-2 1<-3 2<-8 3<-5 4<-5

Args  8 7 7 2 5
Move  0<-8 1<-7 3<-2 4<-5 2<-7

Args  7 7 1 4 9
Move  0<-7 2<-1 3<-4 4<-9 1<-7

Args  7 1 3 9 5
Move  0<-7 2<-3 3<-9 4<-5

Args  8 1 8 2 6
Move  0<-8 3<-2 4<-6 2<-8

Args  2 1 1 2 7
Move  0<-2 3<-2 4<-7 2<-1

Args  7 7 2 7 2
Move  0<-7 1<-7 3<-7 4<-2

Args  1 6 1 7 0
Move  2<-1 3<-7 4<-0 0<-1 1<-6

Args  5 6 1 7 7
Move  0<-5 2<-1 3<-7 4<-7 1<-6

Args  4 2 3 3 6
Move  0<-4 1<-2 2<-3 4<-6

Args  4 5 2 2 2
Move  0<-4 1<-5 3<-2 4<-2

Args  3 8 3 9 7
Move  0<-3 1<-8 2<-3 3<-9 4<-7

Args  8 9 0 4 3
Move  1<-9 2<-0 0<-8 10<-3 3<-4 4<-10

Args  0 5 3 6 8
Move  1<-5 2<-3 3<-6 4<-8

Args  9 5 9 3 3
Move  0<-9 1<-5 2<-9 4<-3

Args  1 1 3 4 2
Move  0<-1 10<-2 2<-3 3<-4 4<-10

Args  5 5 8 8 7
Move  0<-5 1<-5 2<-8 3<-8 4<-7

Args  3 3 8 7 1
Move  0<-3 2<-8 4<-1 1<-3 3<-7

Args  3 7 4 7 4
Move  0<-3 1<-7 2<-4 3<-7

Args  1 7 0 0 3
Move  2<-0 4<-3 3<-0 0<-1 1<-7

Args  4 9 3 5 2
Move  0<-4 1<-9 4<-2 2<-3 3<-5

Args  5 1 6 3 1
Move  0<-5 2<-6 4<-1

Args  5 6 5 1 5
Move  0<-5 2<-5 3<-1 4<-5 1<-6

>>*)

(*[[
!! (SYMFILE #tPermute07 STAMP #tPermute07.%main 1 #tPermute07.m)
!! (CHKSUM STAMP)
!! 
MODULE tPermute07 STAMP 0
IMPORT Random STAMP
IMPORT Out STAMP
ENDHDR

PROC tPermute07.Move 0 4 0
! PROCEDURE Move(i, j: INTEGER);
!   Out.String(" "); Out.Int(i, 0); Out.String("<-"); Out.Int(j, 0);
CONST 2
GLOBAL tPermute07.%5
GLOBAL Out.String
CALL 2
CONST 0
LDLW 12
GLOBAL Out.Int
CALL 2
CONST 3
GLOBAL tPermute07.%1
GLOBAL Out.String
CALL 2
CONST 0
LDLW 16
GLOBAL Out.Int
CALL 2
!   val[i] := val[j]
GLOBAL tPermute07.val
LDLW 16
CONST 11
BOUND 12
LDIW
GLOBAL tPermute07.val
LDLW 12
CONST 11
BOUND 12
STIW
RETURN
END

PROC tPermute07.Permute 36 4 0x00100001
! PROCEDURE Permute(VAR arg: ARRAY OF INTEGER);
!   FOR i := 0 TO N-1 DO u[i] := 0 END;
CONST 0
STLW -4
LABEL L6
LDLW -4
CONST 4
JGT L7
CONST 0
LOCAL -36
LDLW -4
CONST 5
BOUND 22
STIW
INCL -4
JUMP L6
LABEL L7
!   FOR i := 0 TO N-1 DO
CONST 0
STLW -4
LABEL L8
LDLW -4
CONST 4
JGT L9
!     IF arg[i] < N THEN INC(u[arg[i]]) END
LDLW 12
LDLW -4
LDLW 16
BOUND 24
LDIW
CONST 5
JGEQ L12
LOCAL -36
LDLW 12
LDLW -4
LDLW 16
BOUND 24
LDIW
CONST 5
BOUND 24
INDEXW
DUP 0
LOADW
INC
SWAP
STOREW
LABEL L12
!   FOR i := 0 TO N-1 DO
INCL -4
JUMP L8
LABEL L9
!     changed := FALSE;
CONST 0
STLC -13
!     FOR i := 0 TO N-1 DO
CONST 0
STLW -4
LABEL L15
LDLW -4
CONST 4
JGT L16
!       IF (arg[i] # i) & (u[i] = 0) THEN
LDLW 12
LDLW -4
LDLW 16
BOUND 31
LDIW
LDLW -4
JEQ L19
LOCAL -36
LDLW -4
CONST 5
BOUND 31
LDIW
JNEQZ L19
!         Move(i, arg[i]);
LDLW 12
LDLW -4
LDLW 16
BOUND 32
LDIW
LDLW -4
GLOBAL tPermute07.Move
CALL 2
!         IF arg[i] < N THEN DEC(u[arg[i]]) END;
LDLW 12
LDLW -4
LDLW 16
BOUND 33
LDIW
CONST 5
JGEQ L22
LOCAL -36
LDLW 12
LDLW -4
LDLW 16
BOUND 33
LDIW
CONST 5
BOUND 33
INDEXW
DUP 0
LOADW
DEC
SWAP
STOREW
LABEL L22
!         arg[i] := i; changed := TRUE
LDLW -4
LDLW 12
LDLW -4
LDLW 16
BOUND 34
STIW
CONST 1
STLC -13
LABEL L19
!     FOR i := 0 TO N-1 DO
INCL -4
JUMP L15
LABEL L16
!   UNTIL ~changed;
LDLC -13
JNEQZ L9
!   FOR i := 0 TO N-1 DO
CONST 0
STLW -4
LABEL L24
LDLW -4
CONST 4
JGT L25
!     IF arg[i] # i THEN
LDLW 12
LDLW -4
LDLW 16
BOUND 41
LDIW
LDLW -4
JEQ L28
!       Move(2*N, i);
LDLW -4
CONST 10
GLOBAL tPermute07.Move
CALL 2
!       j := i;
LDLW -4
STLW -8
LABEL L29
!       WHILE arg[j] # i DO
LDLW 12
LDLW -8
LDLW 16
BOUND 44
LDIW
LDLW -4
JEQ L31
!         Move(j, arg[j]);
LDLW 12
LDLW -8
LDLW 16
BOUND 45
LDIW
LDLW -8
GLOBAL tPermute07.Move
CALL 2
!         k := arg[j]; arg[j] := j; j := k
LDLW 12
LDLW -8
LDLW 16
BOUND 46
LDIW
STLW -12
LDLW -8
LDLW 12
LDLW -8
LDLW 16
BOUND 46
STIW
LDLW -12
STLW -8
JUMP L29
LABEL L31
!       Move(j, 2*N);
CONST 10
LDLW -8
GLOBAL tPermute07.Move
CALL 2
!       arg[j] := j
LDLW -8
LDLW 12
LDLW -8
LDLW 16
BOUND 49
STIW
LABEL L28
!   FOR i := 0 TO N-1 DO
INCL -4
JUMP L24
LABEL L25
RETURN
END

PROC tPermute07.Test 44 4 0
! PROCEDURE Test;
!   FOR i := 0 TO 2*N DO val[i] := Random.Random() END;
CONST 0
STLW -4
LABEL L32
LDLW -4
CONST 10
JGT L33
GLOBAL Random.Random
CALLW 0
GLOBAL tPermute07.val
LDLW -4
CONST 11
BOUND 59
STIW
INCL -4
JUMP L32
LABEL L33
!   Out.String("Args ");
CONST 6
GLOBAL tPermute07.%2
GLOBAL Out.String
CALL 2
!   FOR i := 0 TO N-1 DO
CONST 0
STLW -4
LABEL L34
LDLW -4
CONST 4
JGT L35
!     arg[i] := Random.Roll(2*N);
CONST 10
GLOBAL Random.Roll
CALLW 1
LOCAL -24
LDLW -4
CONST 5
BOUND 63
STIW
!     expected[i] := val[arg[i]];
GLOBAL tPermute07.val
LOCAL -24
LDLW -4
CONST 5
BOUND 64
LDIW
CONST 11
BOUND 64
LDIW
LOCAL -44
LDLW -4
CONST 5
BOUND 64
STIW
!     Out.String(" "); Out.Int(arg[i], 0)
CONST 2
GLOBAL tPermute07.%5
GLOBAL Out.String
CALL 2
CONST 0
LOCAL -24
LDLW -4
CONST 5
BOUND 65
LDIW
GLOBAL Out.Int
CALL 2
!   FOR i := 0 TO N-1 DO
INCL -4
JUMP L34
LABEL L35
!   Out.Ln;
GLOBAL Out.Ln
CALL 0
!   Out.String("Move ");
CONST 6
GLOBAL tPermute07.%3
GLOBAL Out.String
CALL 2
!   Permute(arg);
CONST 5
LOCAL -24
GLOBAL tPermute07.Permute
CALL 2
!   Out.Ln;
GLOBAL Out.Ln
CALL 0
!   FOR i := 0 TO N-1 DO
CONST 0
STLW -4
LABEL L36
LDLW -4
CONST 4
JGT L37
!     IF val[i] # expected[i] THEN
GLOBAL tPermute07.val
LDLW -4
CONST 11
BOUND 74
LDIW
LOCAL -44
LDLW -4
CONST 5
BOUND 74
LDIW
JEQ L40
!       Out.String("***Failed***"); Out.Ln;
CONST 13
GLOBAL tPermute07.%4
GLOBAL Out.String
CALL 2
GLOBAL Out.Ln
CALL 0
LABEL L40
!   FOR i := 0 TO N-1 DO
INCL -4
JUMP L36
LABEL L37
!   Out.Ln
GLOBAL Out.Ln
CALL 0
RETURN
END

PROC tPermute07.Main 4 2 0
! PROCEDURE Main;
!   FOR n := 0 TO 99 DO Test END
CONST 0
STLW -4
LABEL L41
LDLW -4
CONST 99
JGT L42
GLOBAL tPermute07.Test
CALL 0
INCL -4
JUMP L41
LABEL L42
RETURN
END

PROC tPermute07.%main 0 1 0
!   Main
GLOBAL tPermute07.Main
CALL 0
RETURN
END

! Global variables
GLOVAR tPermute07.val 44

! String "<-"
DEFINE tPermute07.%1
STRING 3C2D00

! String "Args "
DEFINE tPermute07.%2
STRING 417267732000

! String "Move "
DEFINE tPermute07.%3
STRING 4D6F76652000

! String "***Failed***"
DEFINE tPermute07.%4
STRING 2A2A2A4661696C65642A2A2A00

! String " "
DEFINE tPermute07.%5
STRING 2000

! End of file
]]*)
