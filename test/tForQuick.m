MODULE tForQuick;

(* Quicksort with a FOR loop *)

IMPORT Random, Out;

PROCEDURE Swap(VAR x, y: REAL);
  VAR t: REAL;
BEGIN
  t := x; x := y; y := t
END Swap;

PROCEDURE Sort(VAR a: ARRAY OF REAL; lo, hi: INTEGER);
  VAR j, k: INTEGER; pivot: REAL;
BEGIN
  IF lo+1 < hi THEN
    pivot := a[lo];
    j := lo;

    (* Inv: a[lo+1..j+1) < pivot and a[j+1..k) >= pivot *)

    FOR k := lo+1 TO hi-1 DO
      IF a[k] < pivot THEN j := j+1; Swap(a[j], a[k]) END
    END;
    Swap(a[lo], a[j]);
    Sort(a, lo, j); Sort(a, j+1, hi)
  END
END Sort;

CONST N = 20;

VAR b: ARRAY N OF REAL;

PROCEDURE Main;
  VAR i: INTEGER;
BEGIN
  FOR i := 0 TO N-1 DO b[i] := Random.Uniform() END;
  Sort(b, 0, N);
  FOR i := 0 TO N-1 DO Out.Real(b[i]); Out.Ln END
END Main;

BEGIN
  Main
END tForQuick.

(*<<
0.0500813
0.0704103
0.0706561
0.0953171
0.105540
0.129891
0.157322
0.165174
0.383834
0.519090
0.520604
0.638730
0.690535
0.704353
0.774786
0.795043
0.834650
0.901363
0.948559
0.986639
>>*)

(*[[
!! (SYMFILE #tForQuick STAMP #tForQuick.%main 1 #tForQuick.m)
!! (CHKSUM STAMP)
!! 
MODULE tForQuick STAMP 0
IMPORT Random STAMP
IMPORT Out STAMP
ENDHDR

PROC tForQuick.Swap 4 2 0x00300001
! PROCEDURE Swap(VAR x, y: REAL);
!   t := x; x := y; y := t
LDLW 12
LOADF
STLF -4
LDLW 16
LOADF
LDLW 12
STOREF
LDLF -4
LDLW 16
STOREF
RETURN
END

PROC tForQuick.Sort 16 5 0x00100001
! PROCEDURE Sort(VAR a: ARRAY OF REAL; lo, hi: INTEGER);
!   IF lo+1 < hi THEN
LDLW 20
INC
LDLW 24
JGEQ L3
!     pivot := a[lo];
LDLW 12
LDLW 20
LDLW 16
BOUND 17
LDIF
STLF -12
!     j := lo;
LDLW 20
STLW -4
!     FOR k := lo+1 TO hi-1 DO
LDLW 24
DEC
STLW -16
LDLW 20
INC
STLW -8
LABEL L4
LDLW -8
LDLW -16
JGT L5
!       IF a[k] < pivot THEN j := j+1; Swap(a[j], a[k]) END
LDLW 12
LDLW -8
LDLW 16
BOUND 23
LDIF
LDLF -12
FJNLT L8
INCL -4
LDLW 12
LDLW -8
LDLW 16
BOUND 23
INDEXW
LDLW 12
LDLW -4
LDLW 16
BOUND 23
INDEXW
GLOBAL tForQuick.Swap
CALL 2
LABEL L8
!     FOR k := lo+1 TO hi-1 DO
INCL -8
JUMP L4
LABEL L5
!     Swap(a[lo], a[j]);
LDLW 12
LDLW -4
LDLW 16
BOUND 25
INDEXW
LDLW 12
LDLW 20
LDLW 16
BOUND 25
INDEXW
GLOBAL tForQuick.Swap
CALL 2
!     Sort(a, lo, j); Sort(a, j+1, hi)
LDLW -4
LDLW 20
LDLW 16
LDLW 12
GLOBAL tForQuick.Sort
CALL 4
LDLW 24
LDLW -4
INC
LDLW 16
LDLW 12
GLOBAL tForQuick.Sort
CALL 4
LABEL L3
RETURN
END

PROC tForQuick.Main 4 5 0
! PROCEDURE Main;
!   FOR i := 0 TO N-1 DO b[i] := Random.Uniform() END;
CONST 0
STLW -4
LABEL L9
LDLW -4
CONST 19
JGT L10
GLOBAL Random.Uniform
CALLF 0
GLOBAL tForQuick.b
LDLW -4
CONST 20
BOUND 37
STIF
INCL -4
JUMP L9
LABEL L10
!   Sort(b, 0, N);
CONST 20
CONST 0
CONST 20
GLOBAL tForQuick.b
GLOBAL tForQuick.Sort
CALL 4
!   FOR i := 0 TO N-1 DO Out.Real(b[i]); Out.Ln END
CONST 0
STLW -4
LABEL L11
LDLW -4
CONST 19
JGT L12
GLOBAL tForQuick.b
LDLW -4
CONST 20
BOUND 39
LDIF
GLOBAL Out.Real
CALL 1
GLOBAL Out.Ln
CALL 0
INCL -4
JUMP L11
LABEL L12
RETURN
END

PROC tForQuick.%main 0 1 0
!   Main
GLOBAL tForQuick.Main
CALL 0
RETURN
END

! Global variables
GLOVAR tForQuick.b 80

! End of file
]]*)
