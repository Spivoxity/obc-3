MODULE tQuicksort;

IMPORT Random, Out;

CONST N = 60;

TYPE vec = ARRAY N OF INTEGER;

PROCEDURE Swap(VAR x, y: INTEGER);
  VAR t: INTEGER;
BEGIN
  t := x; x := y; y := t
END Swap;

PROCEDURE Partition(VAR u: vec; a, b: INTEGER): INTEGER;
  VAR i, j, pivot: INTEGER;
BEGIN
  i := a+1; j := b; pivot := u[a];
  WHILE i < j DO
    WHILE (i < j) & (u[i] < pivot) DO i := i+1 END;
    WHILE (i < j) & (u[j-1] > pivot) DO j := j-1 END;
    IF i < j THEN 
      Swap(u[i], u[j-1]); 
      i := i+1; j := j-1 
    END
  END;
  (* |u[a+1..i) <= pivot| and |u[j..b) >= pivot| and |j <= i| *)
  Swap(u[a], u[i-1]);
  RETURN i-1
END Partition;

(* Quicksort -- sort |u[a..b)| *)
PROCEDURE Quicksort(VAR u: vec; a, b: INTEGER);
  VAR k: INTEGER;
BEGIN
  IF b - a > 1 THEN
    k := Partition(u, a, b);
    Quicksort(u, a, k);
    Quicksort(u, k+1, b);
  END
END Quicksort;

PROCEDURE Sort(VAR u: vec);
BEGIN
  Quicksort(u, 0, N)
END Sort;

PROCEDURE OneSort(VAR u: vec);
  VAR i, j, k, p: INTEGER;
BEGIN
  (* Cf. Knuth and Huang Bing-Chao, "A one-way, stackless Quicksort 
     algorithm", in Selected Papers on Design of Algorithms.  I removed
     the sentinel and recast the loop first as a LOOP then as a WHILE. *)
  FOR k := 0 TO N-1 DO
    WHILE u[k] >= 0 DO
      j := k+1; i := k; p := u[k];
      WHILE (j < N) & (u[j] >= 0) DO
        IF u[j] < p THEN
          u[i] := u[j]; i := i+1; u[j] := u[i]
        END;
        j := j+1
      END;
      u[i] := -p-1
    END;
    u[k] := -u[k]-1
  END
END OneSort;    

PROCEDURE Test(sort: PROCEDURE (VAR u: vec));
  VAR i: INTEGER; a: vec;
BEGIN
  FOR i := 0 TO N-1 DO a[i] := Random.Roll(10) END;
  FOR i := 0 TO N-1 DO Out.Int(a[i], 0) END; Out.Ln;
  sort(a);
  FOR i := 0 TO N-1 DO Out.Int(a[i], 0) END; Out.Ln
END Test;

BEGIN
  Test(Sort);
  Test(OneSort)
END tQuicksort.

(*<<
119978306115907675004286403777538494004382883317447914012890
000000000111111122233333344444444555666777777778888888999999
620173403342128490291000852607078642393887930158832751590364
000000000011111222222233333333444445555666677777888888899999
>>*)

(*[[
!! (SYMFILE #tQuicksort STAMP #tQuicksort.%main 1 #tQuicksort.m)
!! (CHKSUM STAMP)
!! 
MODULE tQuicksort STAMP 0
IMPORT Random STAMP
IMPORT Out STAMP
ENDHDR

PROC tQuicksort.Swap 4 2 0x00300001
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

PROC tQuicksort.Partition 12 4 0x00100001
! PROCEDURE Partition(VAR u: vec; a, b: INTEGER): INTEGER;
!   i := a+1; j := b; pivot := u[a];
LDLW 16
INC
STLW -4
LDLW 20
STLW -8
LDLW 12
LDLW 16
CONST 60
BOUND 18
LDIW
STLW -12
LABEL L1
!   WHILE i < j DO
LDLW -4
LDLW -8
JGEQ L3
LABEL L4
!     WHILE (i < j) & (u[i] < pivot) DO i := i+1 END;
LDLW -4
LDLW -8
JGEQ L6
LDLW 12
LDLW -4
CONST 60
BOUND 20
LDIW
LDLW -12
JGEQ L6
INCL -4
JUMP L4
LABEL L6
!     WHILE (i < j) & (u[j-1] > pivot) DO j := j-1 END;
LDLW -4
LDLW -8
JGEQ L10
LDLW 12
LDLW -8
DEC
CONST 60
BOUND 21
LDIW
LDLW -12
JLEQ L10
DECL -8
JUMP L6
LABEL L10
!     IF i < j THEN 
LDLW -4
LDLW -8
JGEQ L1
!       Swap(u[i], u[j-1]); 
LDLW 12
LDLW -8
DEC
CONST 60
BOUND 23
INDEXW
LDLW 12
LDLW -4
CONST 60
BOUND 23
INDEXW
GLOBAL tQuicksort.Swap
CALL 2
!       i := i+1; j := j-1 
INCL -4
DECL -8
JUMP L1
LABEL L3
!   Swap(u[a], u[i-1]);
LDLW 12
LDLW -4
DEC
CONST 60
BOUND 28
INDEXW
LDLW 12
LDLW 16
CONST 60
BOUND 28
INDEXW
GLOBAL tQuicksort.Swap
CALL 2
!   RETURN i-1
LDLW -4
DEC
RETURN
END

PROC tQuicksort.Quicksort 4 4 0x00100001
! PROCEDURE Quicksort(VAR u: vec; a, b: INTEGER);
!   IF b - a > 1 THEN
LDLW 20
LDLW 16
MINUS
CONST 1
JLEQ L17
!     k := Partition(u, a, b);
LDLW 20
LDLW 16
LDLW 12
GLOBAL tQuicksort.Partition
CALLW 3
STLW -4
!     Quicksort(u, a, k);
LDLW -4
LDLW 16
LDLW 12
GLOBAL tQuicksort.Quicksort
CALL 3
!     Quicksort(u, k+1, b);
LDLW 20
LDLW -4
INC
LDLW 12
GLOBAL tQuicksort.Quicksort
CALL 3
LABEL L17
RETURN
END

PROC tQuicksort.Sort 0 4 0x00100001
! PROCEDURE Sort(VAR u: vec);
!   Quicksort(u, 0, N)
CONST 60
CONST 0
LDLW 12
GLOBAL tQuicksort.Quicksort
CALL 3
RETURN
END

PROC tQuicksort.OneSort 16 4 0x00100001
! PROCEDURE OneSort(VAR u: vec);
!   FOR k := 0 TO N-1 DO
CONST 0
STLW -12
LABEL L18
LDLW -12
CONST 59
JGT L19
LABEL L20
!     WHILE u[k] >= 0 DO
LDLW 12
LDLW -12
CONST 60
BOUND 55
LDIW
JLTZ L22
!       j := k+1; i := k; p := u[k];
LDLW -12
INC
STLW -8
LDLW -12
STLW -4
LDLW 12
LDLW -12
CONST 60
BOUND 56
LDIW
STLW -16
LABEL L23
!       WHILE (j < N) & (u[j] >= 0) DO
LDLW -8
CONST 60
JGEQ L25
LDLW 12
LDLW -8
CONST 60
BOUND 57
LDIW
JLTZ L25
!         IF u[j] < p THEN
LDLW 12
LDLW -8
CONST 60
BOUND 58
LDIW
LDLW -16
JGEQ L28
!           u[i] := u[j]; i := i+1; u[j] := u[i]
LDLW 12
LDLW -8
CONST 60
BOUND 59
LDIW
LDLW 12
LDLW -4
CONST 60
BOUND 59
STIW
INCL -4
LDLW 12
LDLW -4
CONST 60
BOUND 59
LDIW
LDLW 12
LDLW -8
CONST 60
BOUND 59
STIW
LABEL L28
!         j := j+1
INCL -8
JUMP L23
LABEL L25
!       u[i] := -p-1
LDLW -16
UMINUS
DEC
LDLW 12
LDLW -4
CONST 60
BOUND 63
STIW
JUMP L20
LABEL L22
!     u[k] := -u[k]-1
LDLW 12
LDLW -12
CONST 60
BOUND 65
LDIW
UMINUS
DEC
LDLW 12
LDLW -12
CONST 60
BOUND 65
STIW
!   FOR k := 0 TO N-1 DO
INCL -12
JUMP L18
LABEL L19
RETURN
END

PROC tQuicksort.Test 244 4 0
! PROCEDURE Test(sort: PROCEDURE (VAR u: vec));
!   FOR i := 0 TO N-1 DO a[i] := Random.Roll(10) END;
CONST 0
STLW -4
LABEL L30
LDLW -4
CONST 59
JGT L31
CONST 10
GLOBAL Random.Roll
CALLW 1
LOCAL -244
LDLW -4
CONST 60
BOUND 72
STIW
INCL -4
JUMP L30
LABEL L31
!   FOR i := 0 TO N-1 DO Out.Int(a[i], 0) END; Out.Ln;
CONST 0
STLW -4
LABEL L32
LDLW -4
CONST 59
JGT L33
CONST 0
LOCAL -244
LDLW -4
CONST 60
BOUND 73
LDIW
GLOBAL Out.Int
CALL 2
INCL -4
JUMP L32
LABEL L33
GLOBAL Out.Ln
CALL 0
!   sort(a);
LOCAL -244
LDLW 16
STATLINK
LDLW 12
NCHECK 74
CALL 1
!   FOR i := 0 TO N-1 DO Out.Int(a[i], 0) END; Out.Ln
CONST 0
STLW -4
LABEL L34
LDLW -4
CONST 59
JGT L35
CONST 0
LOCAL -244
LDLW -4
CONST 60
BOUND 75
LDIW
GLOBAL Out.Int
CALL 2
INCL -4
JUMP L34
LABEL L35
GLOBAL Out.Ln
CALL 0
RETURN
END

PROC tQuicksort.%main 0 3 0
!   Test(Sort);
CONST 0
GLOBAL tQuicksort.Sort
GLOBAL tQuicksort.Test
CALL 2
!   Test(OneSort)
CONST 0
GLOBAL tQuicksort.OneSort
GLOBAL tQuicksort.Test
CALL 2
RETURN
END

! End of file
]]*)
