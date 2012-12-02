MODULE tMissing;

IMPORT Random, Out;

CONST M = 1000; N = M+1;

VAR a: ARRAY M+2 OF INTEGER;

PROCEDURE Partition(m, n: INTEGER; x: INTEGER): INTEGER;
  VAR i, j: INTEGER; t: INTEGER;
BEGIN
  i := m; j := n;
  WHILE i < j DO
    IF a[i] < x THEN
      i := i+1
    ELSE
      j := j-1;
      t := a[i]; a[i] := a[j]; a[j] := t
    END
  END;
  RETURN i
END Partition;

PROCEDURE Missing(): INTEGER;
  VAR p, q, r, m, k: INTEGER;
BEGIN
  p := 0; q := M; r := N;

  (* Inv: a[0..p) contains [0..p), but
      a[p..q) is a proper subset of [p..r) *)
  WHILE p # q DO
    m := (p+r) DIV 2;
    k := Partition(p, q, m);
    IF k < m THEN
      q := k; r := m
    ELSE
      p := k
    END
  END;

  RETURN p
END Missing;

PROCEDURE Swap(VAR x, y: INTEGER);
  VAR t: INTEGER;
BEGIN
  t := x; x := y; y := t
END Swap;

PROCEDURE Bundala(): INTEGER;
  VAR n: INTEGER;
BEGIN
  n := 0;
  WHILE n < M DO
    WHILE (a[n] < M) & (a[n] # n) DO
      Swap(a[n], a[a[n]])
    END;
    n := n + 1
  END;

  n := 0;
  WHILE (n < M) & (a[n] = n) DO n := n+1 END;
  RETURN n
END Bundala;

PROCEDURE Bundala2(): INTEGER;
  CONST n = M+1;
BEGIN
  WHILE a[a[n]] # a[n] DO
    Swap(a[n], a[a[n]])
  END;
  RETURN a[n]
END Bundala2;

PROCEDURE Permute(m: INTEGER);
  VAR i, j: INTEGER; t: INTEGER;
BEGIN
  FOR i := m-1 TO 1 BY -1 DO
    j := Random.Roll(i+1);
    ASSERT((j >= 0) & (j <= i));
    t := a[i]; a[i] := a[j]; a[j] := t
  END
END Permute;

PROCEDURE Test;
  VAR x, i, r: INTEGER;
BEGIN
  x := Random.Roll(N);
  Out.Int(x, 0); Out.Ln;
  i := 0;
  FOR r := 0 TO N-1 DO
    IF r # x THEN a[i] := r; i := i+1 END
  END;

  Permute(M);
  x := Missing();  
  Out.Int(x, 0); Out.Ln;

  Permute(M);
  x := Bundala();
  Out.Int(x, 0); Out.Ln;

  a[M] := x; x := Random.Roll(N); a[M+1] := x;
  Out.Int(x, 0); Out.Ln;
  Permute(M+2);
  x := Bundala2();
  Out.Int(x, 0); Out.Ln
END Test;

BEGIN
  Test
END tMissing.   

(*<<
165
165
165
380
380
>>*)

(*[[
!! SYMFILE #tMissing STAMP #tMissing.%main 1
!! END STAMP
!! 
MODULE tMissing STAMP 0
IMPORT Random STAMP
IMPORT Out STAMP
ENDHDR

PROC tMissing.Partition 3 16 0
! PROCEDURE Partition(m, n: INTEGER; x: INTEGER): INTEGER;
!   i := m; j := n;
LDLW 12
STLW -4
LDLW 16
STLW -8
JUMP 3
LABEL 1
!     IF a[i] < x THEN
CONST tMissing.a
LDLW -4
CONST 1002
BOUND 14
LDIW
LDLW 20
JGEQ 4
!       i := i+1
INCL -4
JUMP 3
LABEL 4
!       j := j-1;
DECL -8
!       t := a[i]; a[i] := a[j]; a[j] := t
CONST tMissing.a
LDLW -4
CONST 1002
BOUND 18
LDIW
STLW -12
CONST tMissing.a
LDLW -8
CONST 1002
BOUND 18
LDIW
CONST tMissing.a
LDLW -4
CONST 1002
BOUND 18
STIW
LDLW -12
CONST tMissing.a
LDLW -8
CONST 1002
BOUND 18
STIW
LABEL 3
!   WHILE i < j DO
LDLW -4
LDLW -8
JLT 1
!   RETURN i
LDLW -4
RETURNW
END

PROC tMissing.Missing 5 16 0
! PROCEDURE Missing(): INTEGER;
!   p := 0; q := M; r := N;
CONST 0
STLW -4
CONST 1000
STLW -8
CONST 1001
STLW -12
JUMP 7
LABEL 5
!     m := (p+r) DIV 2;
LDLW -4
LDLW -12
PLUS
CONST 2
DIV
STLW -16
!     k := Partition(p, q, m);
LDLW -16
LDLW -8
LDLW -4
CONST tMissing.Partition
CALLW 3
STLW -20
!     IF k < m THEN
LDLW -20
LDLW -16
JGEQ 8
!       q := k; r := m
LDLW -20
STLW -8
LDLW -16
STLW -12
JUMP 7
LABEL 8
!       p := k
LDLW -20
STLW -4
LABEL 7
!   WHILE p # q DO
LDLW -4
LDLW -8
JNEQ 5
!   RETURN p
LDLW -4
RETURNW
END

PROC tMissing.Swap 1 16 0x00300001
! PROCEDURE Swap(VAR x, y: INTEGER);
!   t := x; x := y; y := t
LDLW 12
LOADW
STLW -4
LDLW 16
LOADW
LDLW 12
STOREW
LDLW -4
LDLW 16
STOREW
RETURN
END

PROC tMissing.Bundala 1 16 0
! PROCEDURE Bundala(): INTEGER;
!   n := 0;
CONST 0
STLW -4
JUMP 10
LABEL 11
!       Swap(a[n], a[a[n]])
CONST tMissing.a
CONST tMissing.a
LDLW -4
CONST 1002
BOUND 56
LDIW
CONST 1002
BOUND 56
INDEXW
CONST tMissing.a
LDLW -4
CONST 1002
BOUND 56
INDEXW
CONST tMissing.Swap
CALL 2
LABEL 12
!     WHILE (a[n] < M) & (a[n] # n) DO
CONST tMissing.a
LDLW -4
CONST 1002
BOUND 55
LDIW
CONST 1000
JGEQ 13
CONST tMissing.a
LDLW -4
CONST 1002
BOUND 55
LDIW
LDLW -4
JNEQ 11
LABEL 13
!     n := n + 1
INCL -4
LABEL 10
!   WHILE n < M DO
LDLW -4
CONST 1000
JLT 12
!   n := 0;
CONST 0
STLW -4
JUMP 15
LABEL 14
INCL -4
LABEL 15
!   WHILE (n < M) & (a[n] = n) DO n := n+1 END;
LDLW -4
CONST 1000
JGEQ 16
CONST tMissing.a
LDLW -4
CONST 1002
BOUND 62
LDIW
LDLW -4
JEQ 14
LABEL 16
!   RETURN n
LDLW -4
RETURNW
END

PROC tMissing.Bundala2 0 16 0
! PROCEDURE Bundala2(): INTEGER;
JUMP 18
LABEL 17
!     Swap(a[n], a[a[n]])
CONST tMissing.a
CONST tMissing.a
LDNW 4004
CONST 1002
BOUND 70
INDEXW
CONST tMissing.a
CONST 4004
PLUSA
CONST tMissing.Swap
CALL 2
LABEL 18
!   WHILE a[a[n]] # a[n] DO
CONST tMissing.a
CONST tMissing.a
LDNW 4004
CONST 1002
BOUND 69
LDIW
CONST tMissing.a
LDNW 4004
JNEQ 17
!   RETURN a[n]
CONST tMissing.a
LDNW 4004
RETURNW
END

PROC tMissing.Permute 3 16 0
! PROCEDURE Permute(m: INTEGER);
!   FOR i := m-1 TO 1 BY -1 DO
LDLW 12
DEC
STLW -4
JUMP 20
LABEL 19
!     j := Random.Roll(i+1);
LDLW -4
INC
CONST Random.Roll
CALLW 1
STLW -8
!     ASSERT((j >= 0) & (j <= i));
LDLW -8
JLTZ 22
LDLW -8
LDLW -4
JLEQ 21
LABEL 22
CONST 0
EASSERT 80
LABEL 21
!     t := a[i]; a[i] := a[j]; a[j] := t
CONST tMissing.a
LDLW -4
CONST 1002
BOUND 81
LDIW
STLW -12
CONST tMissing.a
LDLW -8
CONST 1002
BOUND 81
LDIW
CONST tMissing.a
LDLW -4
CONST 1002
BOUND 81
STIW
LDLW -12
CONST tMissing.a
LDLW -8
CONST 1002
BOUND 81
STIW
!   FOR i := m-1 TO 1 BY -1 DO
DECL -4
LABEL 20
LDLW -4
CONST 1
JGEQ 19
RETURN
END

PROC tMissing.Test 3 16 0
! PROCEDURE Test;
!   x := Random.Roll(N);
CONST 1001
CONST Random.Roll
CALLW 1
STLW -4
!   Out.Int(x, 0); Out.Ln;
CONST 0
LDLW -4
CONST Out.Int
CALL 2
CONST Out.Ln
CALL 0
!   i := 0;
CONST 0
STLW -8
!   FOR r := 0 TO N-1 DO
CONST 0
STLW -12
JUMP 24
LABEL 23
!     IF r # x THEN a[i] := r; i := i+1 END
LDLW -12
LDLW -4
JEQ 26
LDLW -12
CONST tMissing.a
LDLW -8
CONST 1002
BOUND 92
STIW
INCL -8
LABEL 26
!   FOR r := 0 TO N-1 DO
INCL -12
LABEL 24
LDLW -12
CONST 1000
JLEQ 23
!   Permute(M);
CONST 1000
CONST tMissing.Permute
CALL 1
!   x := Missing();  
CONST tMissing.Missing
CALLW 0
STLW -4
!   Out.Int(x, 0); Out.Ln;
CONST 0
LDLW -4
CONST Out.Int
CALL 2
CONST Out.Ln
CALL 0
!   Permute(M);
CONST 1000
CONST tMissing.Permute
CALL 1
!   x := Bundala();
CONST tMissing.Bundala
CALLW 0
STLW -4
!   Out.Int(x, 0); Out.Ln;
CONST 0
LDLW -4
CONST Out.Int
CALL 2
CONST Out.Ln
CALL 0
!   a[M] := x; x := Random.Roll(N); a[M+1] := x;
LDLW -4
CONST tMissing.a
STNW 4000
CONST 1001
CONST Random.Roll
CALLW 1
STLW -4
LDLW -4
CONST tMissing.a
STNW 4004
!   Out.Int(x, 0); Out.Ln;
CONST 0
LDLW -4
CONST Out.Int
CALL 2
CONST Out.Ln
CALL 0
!   Permute(M+2);
CONST 1002
CONST tMissing.Permute
CALL 1
!   x := Bundala2();
CONST tMissing.Bundala2
CALLW 0
STLW -4
!   Out.Int(x, 0); Out.Ln
CONST 0
LDLW -4
CONST Out.Int
CALL 2
CONST Out.Ln
CALL 0
RETURN
END

PROC tMissing.%main 0 16 0
!   Test
CONST tMissing.Test
CALL 0
RETURN
END

! Global variables
GLOBAL tMissing.a 4008

! End of file
]]*)
