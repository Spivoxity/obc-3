MODULE tKnuth;

(* Solve the recurrence f(n) = f(n-1) + f(floor(n/2)), f(0) = 1. *)

IMPORT Out;

CONST N = 10; M = 1000000000;

TYPE bignum = ARRAY N OF INTEGER;

PROCEDURE Normalize(VAR a: bignum);
  VAR i: INTEGER;
BEGIN
  FOR i := 0 TO N-2 DO
    a[i+1] := a[i+1] + a[i] DIV M;
    a[i] := a[i] MOD M
  END
END Normalize;

PROCEDURE Set(VAR a: bignum; x: INTEGER);
  VAR i: INTEGER;
BEGIN
  a[0] := x;
  FOR i := 1 TO N-1 DO a[i] := 0 END;
  Normalize(a)
END Set;

PROCEDURE Add(VAR a1: bignum; a2: bignum);
  VAR i: INTEGER;
BEGIN
  FOR i := 0 TO N-1 DO a1[i] := a1[i] + a2[i] END;
  Normalize(a1)
END Add;

PROCEDURE PrintPiece(x: INTEGER);
  VAR m: INTEGER;
BEGIN
  m := M;
  WHILE m > 1 DO
    m := m DIV 10;
    Out.Int(x DIV m, 0);
    x := x MOD m
  END
END PrintPiece;

PROCEDURE Print(VAR a: bignum);
  VAR i: INTEGER;
BEGIN
  i := N-1;
  WHILE (i > 0) & (a[i] = 0) DO i := i-1 END;
  Out.Int(a[i], 0); i := i-1;
  WHILE i >= 0 DO PrintPiece(a[i]); i := i-1 END
END Print;

PROCEDURE Calc(n: INTEGER; VAR ans: bignum);
  CONST K = 20;
  VAR 
    i, j, k: INTEGER;
    arg: ARRAY K OF INTEGER;
    val: ARRAY K OF bignum;
BEGIN
  FOR j := 0 TO K-1 DO arg[j] := 0; Set(val[j], 1) END;

  (* Invariant: arg[j+1] = arg[j] DIV 2, val[j] = f(arg[j]) *)

  FOR i := 1 TO n DO
    j := 0; k := i;
    WHILE k > arg[j] DO k := k DIV 2; j := j + 1 END;
    WHILE j > 0 DO
      j := j - 1;
      INC(arg[j]);
      Add(val[j], val[j+1])
    END
  END;

  ans := val[0]
END Calc;

VAR ans: bignum;

BEGIN
  Calc(10000, ans);
  Print(ans); Out.Ln
END tKnuth.

(*<<
214454008193526428202
>>*)

(*[[
!! (SYMFILE #tKnuth STAMP #tKnuth.%main 1 #tKnuth.m)
!! (CHKSUM STAMP)
!! 
MODULE tKnuth STAMP 0
IMPORT Out STAMP
ENDHDR

PROC tKnuth.Normalize 4 4 0x00100001
! PROCEDURE Normalize(VAR a: bignum);
!   FOR i := 0 TO N-2 DO
CONST 0
STLW -4
LABEL L1
LDLW -4
CONST 8
JGT L2
!     a[i+1] := a[i+1] + a[i] DIV M;
LDLW 12
LDLW -4
INC
CONST 10
BOUND 15
LDIW
LDLW 12
LDLW -4
CONST 10
BOUND 15
LDIW
CONST 1000000000
DIV
PLUS
LDLW 12
LDLW -4
INC
CONST 10
BOUND 15
STIW
!     a[i] := a[i] MOD M
LDLW 12
LDLW -4
CONST 10
BOUND 16
LDIW
CONST 1000000000
MOD
LDLW 12
LDLW -4
CONST 10
BOUND 16
STIW
!   FOR i := 0 TO N-2 DO
INCL -4
JUMP L1
LABEL L2
RETURN
END

PROC tKnuth.Set 4 4 0x00100001
! PROCEDURE Set(VAR a: bignum; x: INTEGER);
!   a[0] := x;
LDLW 16
LDLW 12
STOREW
!   FOR i := 1 TO N-1 DO a[i] := 0 END;
CONST 1
STLW -4
LABEL L3
LDLW -4
CONST 9
JGT L4
CONST 0
LDLW 12
LDLW -4
CONST 10
BOUND 24
STIW
INCL -4
JUMP L3
LABEL L4
!   Normalize(a)
LDLW 12
GLOBAL tKnuth.Normalize
CALL 1
RETURN
END

PROC tKnuth.Add 44 4 0x00100001
! PROCEDURE Add(VAR a1: bignum; a2: bignum);
LOCAL -44
LDLW 16
CONST 40
FIXCOPY
!   FOR i := 0 TO N-1 DO a1[i] := a1[i] + a2[i] END;
CONST 0
STLW -4
LABEL L5
LDLW -4
CONST 9
JGT L6
LDLW 12
LDLW -4
CONST 10
BOUND 31
LDIW
LOCAL -44
LDLW -4
CONST 10
BOUND 31
LDIW
PLUS
LDLW 12
LDLW -4
CONST 10
BOUND 31
STIW
INCL -4
JUMP L5
LABEL L6
!   Normalize(a1)
LDLW 12
GLOBAL tKnuth.Normalize
CALL 1
RETURN
END

PROC tKnuth.PrintPiece 4 3 0
! PROCEDURE PrintPiece(x: INTEGER);
!   m := M;
CONST 1000000000
STLW -4
LABEL L7
!   WHILE m > 1 DO
LDLW -4
CONST 1
JLEQ L9
!     m := m DIV 10;
LDLW -4
CONST 10
DIV
STLW -4
!     Out.Int(x DIV m, 0);
CONST 0
LDLW 12
LDLW -4
ZCHECK 41
DIV
GLOBAL Out.Int
CALL 2
!     x := x MOD m
LDLW 12
LDLW -4
ZCHECK 42
MOD
STLW 12
JUMP L7
LABEL L9
RETURN
END

PROC tKnuth.Print 4 4 0x00100001
! PROCEDURE Print(VAR a: bignum);
!   i := N-1;
CONST 9
STLW -4
LABEL L10
!   WHILE (i > 0) & (a[i] = 0) DO i := i-1 END;
LDLW -4
JLEQZ L12
LDLW 12
LDLW -4
CONST 10
BOUND 50
LDIW
JNEQZ L12
DECL -4
JUMP L10
LABEL L12
!   Out.Int(a[i], 0); i := i-1;
CONST 0
LDLW 12
LDLW -4
CONST 10
BOUND 51
LDIW
GLOBAL Out.Int
CALL 2
DECL -4
LABEL L14
!   WHILE i >= 0 DO PrintPiece(a[i]); i := i-1 END
LDLW -4
JLTZ L16
LDLW 12
LDLW -4
CONST 10
BOUND 52
LDIW
GLOBAL tKnuth.PrintPiece
CALL 1
DECL -4
JUMP L14
LABEL L16
RETURN
END

PROC tKnuth.Calc 896 4 0x00200001
! PROCEDURE Calc(n: INTEGER; VAR ans: bignum);
!   FOR j := 0 TO K-1 DO arg[j] := 0; Set(val[j], 1) END;
CONST 0
STLW -8
LABEL L17
LDLW -8
CONST 19
JGT L18
CONST 0
LOCAL -92
LDLW -8
CONST 20
BOUND 62
STIW
CONST 1
LOCAL -892
LDLW -8
CONST 20
BOUND 62
CONST 40
TIMES
OFFSET
GLOBAL tKnuth.Set
CALL 2
INCL -8
JUMP L17
LABEL L18
!   FOR i := 1 TO n DO
LDLW 12
STLW -896
CONST 1
STLW -4
LABEL L19
LDLW -4
LDLW -896
JGT L20
!     j := 0; k := i;
CONST 0
STLW -8
LDLW -4
STLW -12
LABEL L21
!     WHILE k > arg[j] DO k := k DIV 2; j := j + 1 END;
LDLW -12
LOCAL -92
LDLW -8
CONST 20
BOUND 68
LDIW
JLEQ L23
LDLW -12
CONST 2
DIV
STLW -12
INCL -8
JUMP L21
LABEL L23
!     WHILE j > 0 DO
LDLW -8
JLEQZ L26
!       j := j - 1;
DECL -8
!       INC(arg[j]);
LOCAL -92
LDLW -8
CONST 20
BOUND 71
INDEXW
DUP 0
LOADW
INC
SWAP
STOREW
!       Add(val[j], val[j+1])
LOCAL -892
LDLW -8
INC
CONST 20
BOUND 72
CONST 40
TIMES
OFFSET
LOCAL -892
LDLW -8
CONST 20
BOUND 72
CONST 40
TIMES
OFFSET
GLOBAL tKnuth.Add
CALL 2
JUMP L23
LABEL L26
!   FOR i := 1 TO n DO
INCL -4
JUMP L19
LABEL L20
!   ans := val[0]
LDLW 16
LOCAL -892
CONST 40
FIXCOPY
RETURN
END

PROC tKnuth.%main 0 3 0
!   Calc(10000, ans);
GLOBAL tKnuth.ans
CONST 10000
GLOBAL tKnuth.Calc
CALL 2
!   Print(ans); Out.Ln
GLOBAL tKnuth.ans
GLOBAL tKnuth.Print
CALL 1
GLOBAL Out.Ln
CALL 0
RETURN
END

! Global variables
GLOVAR tKnuth.ans 40

! End of file
]]*)
