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
!! (SYMFILE #tMissing STAMP #tMissing.%main 1 #tMissing.m)
!! (CHKSUM STAMP)
!! 
MODULE tMissing STAMP 0
IMPORT Random STAMP
IMPORT Out STAMP
ENDHDR

PROC tMissing.Partition 12 4 0
! PROCEDURE Partition(m, n: INTEGER; x: INTEGER): INTEGER;
!   i := m; j := n;
LDLW 12
STLW -4
LDLW 16
STLW -8
LABEL L1
!   WHILE i < j DO
LDLW -4
LDLW -8
JGEQ L3
!     IF a[i] < x THEN
GLOBAL tMissing.a
LDLW -4
CONST 1002
BOUND 14
LDIW
LDLW 20
JGEQ L6
!       i := i+1
INCL -4
JUMP L1
LABEL L6
!       j := j-1;
DECL -8
!       t := a[i]; a[i] := a[j]; a[j] := t
GLOBAL tMissing.a
LDLW -4
CONST 1002
BOUND 18
LDIW
STLW -12
GLOBAL tMissing.a
LDLW -8
CONST 1002
BOUND 18
LDIW
GLOBAL tMissing.a
LDLW -4
CONST 1002
BOUND 18
STIW
LDLW -12
GLOBAL tMissing.a
LDLW -8
CONST 1002
BOUND 18
STIW
JUMP L1
LABEL L3
!   RETURN i
LDLW -4
RETURN
END

PROC tMissing.Missing 20 4 0
! PROCEDURE Missing(): INTEGER;
!   p := 0; q := M; r := N;
CONST 0
STLW -4
CONST 1000
STLW -8
CONST 1001
STLW -12
LABEL L7
!   WHILE p # q DO
LDLW -4
LDLW -8
JEQ L9
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
GLOBAL tMissing.Partition
CALLW 3
STLW -20
!     IF k < m THEN
LDLW -20
LDLW -16
JGEQ L12
!       q := k; r := m
LDLW -20
STLW -8
LDLW -16
STLW -12
JUMP L7
LABEL L12
!       p := k
LDLW -20
STLW -4
JUMP L7
LABEL L9
!   RETURN p
LDLW -4
RETURN
END

PROC tMissing.Swap 4 2 0x00300001
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

PROC tMissing.Bundala 4 4 0
! PROCEDURE Bundala(): INTEGER;
!   n := 0;
CONST 0
STLW -4
LABEL L13
!   WHILE n < M DO
LDLW -4
CONST 1000
JGEQ L15
LABEL L16
!     WHILE (a[n] < M) & (a[n] # n) DO
GLOBAL tMissing.a
LDLW -4
CONST 1002
BOUND 55
LDIW
CONST 1000
JGEQ L18
GLOBAL tMissing.a
LDLW -4
CONST 1002
BOUND 55
LDIW
LDLW -4
JEQ L18
!       Swap(a[n], a[a[n]])
GLOBAL tMissing.a
GLOBAL tMissing.a
LDLW -4
CONST 1002
BOUND 56
LDIW
CONST 1002
BOUND 56
INDEXW
GLOBAL tMissing.a
LDLW -4
CONST 1002
BOUND 56
INDEXW
GLOBAL tMissing.Swap
CALL 2
JUMP L16
LABEL L18
!     n := n + 1
INCL -4
JUMP L13
LABEL L15
!   n := 0;
CONST 0
STLW -4
LABEL L20
!   WHILE (n < M) & (a[n] = n) DO n := n+1 END;
LDLW -4
CONST 1000
JGEQ L22
GLOBAL tMissing.a
LDLW -4
CONST 1002
BOUND 62
LDIW
LDLW -4
JNEQ L22
INCL -4
JUMP L20
LABEL L22
!   RETURN n
LDLW -4
RETURN
END

PROC tMissing.Bundala2 0 4 0
! PROCEDURE Bundala2(): INTEGER;
LABEL L24
!   WHILE a[a[n]] # a[n] DO
GLOBAL tMissing.a
GLOBAL tMissing.a
LDNW 4004
CONST 1002
BOUND 69
LDIW
GLOBAL tMissing.a
LDNW 4004
JEQ L26
!     Swap(a[n], a[a[n]])
GLOBAL tMissing.a
GLOBAL tMissing.a
LDNW 4004
CONST 1002
BOUND 70
INDEXW
GLOBAL tMissing.a
CONST 4004
OFFSET
GLOBAL tMissing.Swap
CALL 2
JUMP L24
LABEL L26
!   RETURN a[n]
GLOBAL tMissing.a
LDNW 4004
RETURN
END

PROC tMissing.Permute 12 4 0
! PROCEDURE Permute(m: INTEGER);
!   FOR i := m-1 TO 1 BY -1 DO
LDLW 12
DEC
STLW -4
LABEL L27
LDLW -4
CONST 1
JLT L28
!     j := Random.Roll(i+1);
LDLW -4
INC
GLOBAL Random.Roll
CALLW 1
STLW -8
!     ASSERT((j >= 0) & (j <= i));
LDLW -8
JLTZ L29
LDLW -8
LDLW -4
JLEQ L30
LABEL L29
CONST 0
CONST 80
GLOBAL EASSERT
CALL 2
LABEL L30
!     t := a[i]; a[i] := a[j]; a[j] := t
GLOBAL tMissing.a
LDLW -4
CONST 1002
BOUND 81
LDIW
STLW -12
GLOBAL tMissing.a
LDLW -8
CONST 1002
BOUND 81
LDIW
GLOBAL tMissing.a
LDLW -4
CONST 1002
BOUND 81
STIW
LDLW -12
GLOBAL tMissing.a
LDLW -8
CONST 1002
BOUND 81
STIW
!   FOR i := m-1 TO 1 BY -1 DO
DECL -4
JUMP L27
LABEL L28
RETURN
END

PROC tMissing.Test 12 4 0
! PROCEDURE Test;
!   x := Random.Roll(N);
CONST 1001
GLOBAL Random.Roll
CALLW 1
STLW -4
!   Out.Int(x, 0); Out.Ln;
CONST 0
LDLW -4
GLOBAL Out.Int
CALL 2
GLOBAL Out.Ln
CALL 0
!   i := 0;
CONST 0
STLW -8
!   FOR r := 0 TO N-1 DO
CONST 0
STLW -12
LABEL L32
LDLW -12
CONST 1000
JGT L33
!     IF r # x THEN a[i] := r; i := i+1 END
LDLW -12
LDLW -4
JEQ L36
LDLW -12
GLOBAL tMissing.a
LDLW -8
CONST 1002
BOUND 92
STIW
INCL -8
LABEL L36
!   FOR r := 0 TO N-1 DO
INCL -12
JUMP L32
LABEL L33
!   Permute(M);
CONST 1000
GLOBAL tMissing.Permute
CALL 1
!   x := Missing();  
GLOBAL tMissing.Missing
CALLW 0
STLW -4
!   Out.Int(x, 0); Out.Ln;
CONST 0
LDLW -4
GLOBAL Out.Int
CALL 2
GLOBAL Out.Ln
CALL 0
!   Permute(M);
CONST 1000
GLOBAL tMissing.Permute
CALL 1
!   x := Bundala();
GLOBAL tMissing.Bundala
CALLW 0
STLW -4
!   Out.Int(x, 0); Out.Ln;
CONST 0
LDLW -4
GLOBAL Out.Int
CALL 2
GLOBAL Out.Ln
CALL 0
!   a[M] := x; x := Random.Roll(N); a[M+1] := x;
LDLW -4
GLOBAL tMissing.a
STNW 4000
CONST 1001
GLOBAL Random.Roll
CALLW 1
STLW -4
LDLW -4
GLOBAL tMissing.a
STNW 4004
!   Out.Int(x, 0); Out.Ln;
CONST 0
LDLW -4
GLOBAL Out.Int
CALL 2
GLOBAL Out.Ln
CALL 0
!   Permute(M+2);
CONST 1002
GLOBAL tMissing.Permute
CALL 1
!   x := Bundala2();
GLOBAL tMissing.Bundala2
CALLW 0
STLW -4
!   Out.Int(x, 0); Out.Ln
CONST 0
LDLW -4
GLOBAL Out.Int
CALL 2
GLOBAL Out.Ln
CALL 0
RETURN
END

PROC tMissing.%main 0 1 0
!   Test
GLOBAL tMissing.Test
CALL 0
RETURN
END

! Global variables
GLOVAR tMissing.a 4008

! End of file
]]*)
