MODULE tSelect;

IMPORT Random, Out;

CONST N = 100;

VAR k: INTEGER;
  a: ARRAY N OF INTEGER;

PROCEDURE Randomize;
  VAR i: INTEGER;
BEGIN
  FOR i := 0 TO N-1 DO a[i] := Random.Random() END;
  k := Random.Roll(N)
END Randomize;

PROCEDURE Sort(VAR u: ARRAY OF INTEGER; N: INTEGER);
  VAR r, j, t: INTEGER;
BEGIN
  r := 0;
  WHILE r < N DO
    j := r; t := u[r];
    WHILE (j > 0) & (u[j-1] > t) DO
      u[j] := u[j-1]; j := j-1
    END;
    u[j] := t;
    r := r+1
  END
END Sort;
  
PROCEDURE Swap(VAR a, b: INTEGER);
  VAR t: INTEGER;
BEGIN
  t := a; a := b; b := t
END Swap;

PROCEDURE Partition(m, n: INTEGER): INTEGER;
  VAR i, j, pivot: INTEGER;
BEGIN
  i := m + Random.Roll(n-m);
  pivot := a[i]; a[i] := a[n-1];

  i := m; j := n-1;
  WHILE i < j DO
    IF a[i] < pivot THEN
      i := i+1
    ELSE
      j := j-1;
      Swap(a[i], a[j])
    END
  END;

  a[n-1] := a[i]; a[i] := pivot;
  RETURN i
END Partition;

PROCEDURE Select(k, m, n: INTEGER): INTEGER;
  VAR r: INTEGER;
BEGIN
  IF n - m = 1 THEN
    ASSERT(k = 0); RETURN a[m]
  ELSE
    r := Partition(m, n);
    IF r = m+k THEN
      RETURN a[r]
    ELSIF r > m+k THEN
      RETURN Select(k, m, r)
    ELSE
      RETURN Select(m+k-r-1, r+1, n)
    END
  END
END Select;

PROCEDURE Select2(k, m, n: INTEGER): INTEGER;
  VAR r: INTEGER;
BEGIN
  LOOP
    IF n - m = 1 THEN
      RETURN a[m]
    ELSE
      r := Partition(m, n);
      IF r = k THEN
	RETURN a[r]
      ELSIF r > k THEN
	n := r
      ELSE
        m := r+1
      END
    END
  END
END Select2;

BEGIN
  Randomize;
  Out.Int(Select(k, 0, N), 0); Out.Ln;
  Sort(a, N); Out.Int(a[k], 0); Out.Ln;
  Randomize;
  Out.Int(Select2(k, 0, N), 0); Out.Ln;
  Sort(a, N); Out.Int(a[k], 0); Out.Ln
END tSelect.
    
(*<<
1866793936
1866793936
216254551
216254551
>>*)

(*[[
!! (SYMFILE #tSelect STAMP #tSelect.%main 1 #tSelect.m)
!! (CHKSUM STAMP)
!! 
MODULE tSelect STAMP 0
IMPORT Random STAMP
IMPORT Out STAMP
ENDHDR

PROC tSelect.Randomize 4 4 0
! PROCEDURE Randomize;
!   FOR i := 0 TO N-1 DO a[i] := Random.Random() END;
CONST 0
STLW -4
LABEL L1
LDLW -4
CONST 99
JGT L2
GLOBAL Random.Random
CALLW 0
GLOBAL tSelect.a
LDLW -4
CONST 100
BOUND 13
STIW
INCL -4
JUMP L1
LABEL L2
!   k := Random.Roll(N)
CONST 100
GLOBAL Random.Roll
CALLW 1
STGW tSelect.k
RETURN
END

PROC tSelect.Sort 12 4 0x00100001
! PROCEDURE Sort(VAR u: ARRAY OF INTEGER; N: INTEGER);
!   r := 0;
CONST 0
STLW -4
LABEL L3
!   WHILE r < N DO
LDLW -4
LDLW 20
JGEQ L5
!     j := r; t := u[r];
LDLW -4
STLW -8
LDLW 12
LDLW -4
LDLW 16
BOUND 22
LDIW
STLW -12
LABEL L6
!     WHILE (j > 0) & (u[j-1] > t) DO
LDLW -8
JLEQZ L8
LDLW 12
LDLW -8
DEC
LDLW 16
BOUND 23
LDIW
LDLW -12
JLEQ L8
!       u[j] := u[j-1]; j := j-1
LDLW 12
LDLW -8
DEC
LDLW 16
BOUND 24
LDIW
LDLW 12
LDLW -8
LDLW 16
BOUND 24
STIW
DECL -8
JUMP L6
LABEL L8
!     u[j] := t;
LDLW -12
LDLW 12
LDLW -8
LDLW 16
BOUND 26
STIW
!     r := r+1
INCL -4
JUMP L3
LABEL L5
RETURN
END

PROC tSelect.Swap 4 2 0x00300001
! PROCEDURE Swap(VAR a, b: INTEGER);
!   t := a; a := b; b := t
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

PROC tSelect.Partition 12 4 0
! PROCEDURE Partition(m, n: INTEGER): INTEGER;
!   i := m + Random.Roll(n-m);
LDLW 12
LDLW 16
LDLW 12
MINUS
GLOBAL Random.Roll
CALLW 1
PLUS
STLW -4
!   pivot := a[i]; a[i] := a[n-1];
GLOBAL tSelect.a
LDLW -4
CONST 100
BOUND 41
LDIW
STLW -12
GLOBAL tSelect.a
LDLW 16
DEC
CONST 100
BOUND 41
LDIW
GLOBAL tSelect.a
LDLW -4
CONST 100
BOUND 41
STIW
!   i := m; j := n-1;
LDLW 12
STLW -4
LDLW 16
DEC
STLW -8
LABEL L10
!   WHILE i < j DO
LDLW -4
LDLW -8
JGEQ L12
!     IF a[i] < pivot THEN
GLOBAL tSelect.a
LDLW -4
CONST 100
BOUND 45
LDIW
LDLW -12
JGEQ L15
!       i := i+1
INCL -4
JUMP L10
LABEL L15
!       j := j-1;
DECL -8
!       Swap(a[i], a[j])
GLOBAL tSelect.a
LDLW -8
CONST 100
BOUND 49
INDEXW
GLOBAL tSelect.a
LDLW -4
CONST 100
BOUND 49
INDEXW
GLOBAL tSelect.Swap
CALL 2
JUMP L10
LABEL L12
!   a[n-1] := a[i]; a[i] := pivot;
GLOBAL tSelect.a
LDLW -4
CONST 100
BOUND 53
LDIW
GLOBAL tSelect.a
LDLW 16
DEC
CONST 100
BOUND 53
STIW
LDLW -12
GLOBAL tSelect.a
LDLW -4
CONST 100
BOUND 53
STIW
!   RETURN i
LDLW -4
RETURN
END

PROC tSelect.Select 4 4 0
! PROCEDURE Select(k, m, n: INTEGER): INTEGER;
!   IF n - m = 1 THEN
LDLW 20
LDLW 16
MINUS
CONST 1
JNEQ L23
!     ASSERT(k = 0); RETURN a[m]
LDLW 12
JEQZ L25
CONST 0
CONST 61
GLOBAL EASSERT
CALL 2
LABEL L25
GLOBAL tSelect.a
LDLW 16
CONST 100
BOUND 61
LDIW
RETURN
LABEL L23
!     r := Partition(m, n);
LDLW 20
LDLW 16
GLOBAL tSelect.Partition
CALLW 2
STLW -4
!     IF r = m+k THEN
LDLW -4
LDLW 16
LDLW 12
PLUS
JNEQ L19
!       RETURN a[r]
GLOBAL tSelect.a
LDLW -4
CONST 100
BOUND 65
LDIW
RETURN
LABEL L19
!     ELSIF r > m+k THEN
LDLW -4
LDLW 16
LDLW 12
PLUS
JLEQ L21
!       RETURN Select(k, m, r)
LDLW -4
LDLW 16
LDLW 12
GLOBAL tSelect.Select
CALLW 3
RETURN
LABEL L21
!       RETURN Select(m+k-r-1, r+1, n)
LDLW 20
LDLW -4
INC
LDLW 16
LDLW 12
PLUS
LDLW -4
MINUS
DEC
GLOBAL tSelect.Select
CALLW 3
RETURN
END

PROC tSelect.Select2 4 3 0
! PROCEDURE Select2(k, m, n: INTEGER): INTEGER;
LABEL L26
!     IF n - m = 1 THEN
LDLW 20
LDLW 16
MINUS
CONST 1
JNEQ L35
!       RETURN a[m]
GLOBAL tSelect.a
LDLW 16
CONST 100
BOUND 79
LDIW
RETURN
LABEL L35
!       r := Partition(m, n);
LDLW 20
LDLW 16
GLOBAL tSelect.Partition
CALLW 2
STLW -4
!       IF r = k THEN
LDLW -4
LDLW 12
JNEQ L31
! 	RETURN a[r]
GLOBAL tSelect.a
LDLW -4
CONST 100
BOUND 83
LDIW
RETURN
LABEL L31
!       ELSIF r > k THEN
LDLW -4
LDLW 12
JLEQ L33
! 	n := r
LDLW -4
STLW 20
JUMP L26
LABEL L33
!         m := r+1
LDLW -4
INC
STLW 16
JUMP L26
END

PROC tSelect.%main 0 5 0
!   Randomize;
GLOBAL tSelect.Randomize
CALL 0
!   Out.Int(Select(k, 0, N), 0); Out.Ln;
CONST 0
CONST 100
CONST 0
LDGW tSelect.k
GLOBAL tSelect.Select
CALLW 3
GLOBAL Out.Int
CALL 2
GLOBAL Out.Ln
CALL 0
!   Sort(a, N); Out.Int(a[k], 0); Out.Ln;
CONST 100
CONST 100
GLOBAL tSelect.a
GLOBAL tSelect.Sort
CALL 3
CONST 0
GLOBAL tSelect.a
LDGW tSelect.k
CONST 100
BOUND 96
LDIW
GLOBAL Out.Int
CALL 2
GLOBAL Out.Ln
CALL 0
!   Randomize;
GLOBAL tSelect.Randomize
CALL 0
!   Out.Int(Select2(k, 0, N), 0); Out.Ln;
CONST 0
CONST 100
CONST 0
LDGW tSelect.k
GLOBAL tSelect.Select2
CALLW 3
GLOBAL Out.Int
CALL 2
GLOBAL Out.Ln
CALL 0
!   Sort(a, N); Out.Int(a[k], 0); Out.Ln
CONST 100
CONST 100
GLOBAL tSelect.a
GLOBAL tSelect.Sort
CALL 3
CONST 0
GLOBAL tSelect.a
LDGW tSelect.k
CONST 100
BOUND 99
LDIW
GLOBAL Out.Int
CALL 2
GLOBAL Out.Ln
CALL 0
RETURN
END

! Global variables
GLOVAR tSelect.k 4
GLOVAR tSelect.a 400

! End of file
]]*)
