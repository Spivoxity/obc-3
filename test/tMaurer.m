(* Maurer, Letter to the editor, CACM *)

MODULE tMaurer;

IMPORT Random, Out;

PROCEDURE Sort(VAR a: ARRAY OF INTEGER);
  VAR i, t: INTEGER;
BEGIN
  i := 0;
  REPEAT
    IF a[i] <= a[i+1] THEN
      i := i+1
    ELSE
      t := a[i]; a[i] := a[i+1]; a[i+1] := t;
      IF i = 0 THEN
        i := i+1
      ELSE
        i := i-1
      END
    END
  UNTIL i = LEN(a)-1
END Sort;

VAR j: INTEGER; b: ARRAY 20 OF INTEGER;

BEGIN
  FOR j := 0 TO LEN(b)-1 DO
    b[j] := Random.Random()
  END;

  Sort(b);

  FOR j := 0 TO LEN(b)-1 DO
    Out.Int(b[j], 0); Out.Ln
  END
END tMaurer.
      
(*<<
107548793
151204932
151732736
204691840
226645324
278937913
337847102
354709164
824278112
1114736986
1117989450
1371663186
1482911733
1512587038
1663840066
1707341839
1792396945
1935662791
2037015380
2118791974
>>*)

(*[[
!! (SYMFILE #tMaurer STAMP #tMaurer.%main 3 #tMaurer.m)
!! (CHKSUM STAMP)
!! 
MODULE tMaurer STAMP 0
IMPORT Random STAMP
IMPORT Out STAMP
ENDHDR

PROC tMaurer.Sort 8 4 0x00100001
! PROCEDURE Sort(VAR a: ARRAY OF INTEGER);
!   i := 0;
CONST 0
STLW -4
LABEL L1
!     IF a[i] <= a[i+1] THEN
LDLW 12
LDLW -4
LDLW 16
BOUND 12
LDIW
LDLW 12
LDLW -4
INC
LDLW 16
BOUND 12
LDIW
JGT L8
!       i := i+1
INCL -4
JUMP L4
LABEL L8
!       t := a[i]; a[i] := a[i+1]; a[i+1] := t;
LDLW 12
LDLW -4
LDLW 16
BOUND 15
LDIW
STLW -8
LDLW 12
LDLW -4
INC
LDLW 16
BOUND 15
LDIW
LDLW 12
LDLW -4
LDLW 16
BOUND 15
STIW
LDLW -8
LDLW 12
LDLW -4
INC
LDLW 16
BOUND 15
STIW
!       IF i = 0 THEN
LDLW -4
JNEQZ L6
!         i := i+1
INCL -4
JUMP L4
LABEL L6
!         i := i-1
DECL -4
LABEL L4
!   UNTIL i = LEN(a)-1
LDLW -4
LDLW 16
DEC
JNEQ L1
RETURN
END

PROC tMaurer.%main 0 4 0
!   FOR j := 0 TO LEN(b)-1 DO
CONST 0
STGW tMaurer.j
LABEL L9
LDGW tMaurer.j
CONST 19
JGT L10
!     b[j] := Random.Random()
GLOBAL Random.Random
CALLW 0
GLOBAL tMaurer.b
LDGW tMaurer.j
CONST 20
BOUND 29
STIW
!   FOR j := 0 TO LEN(b)-1 DO
LDGW tMaurer.j
INC
STGW tMaurer.j
JUMP L9
LABEL L10
!   Sort(b);
CONST 20
GLOBAL tMaurer.b
GLOBAL tMaurer.Sort
CALL 2
!   FOR j := 0 TO LEN(b)-1 DO
CONST 0
STGW tMaurer.j
LABEL L11
LDGW tMaurer.j
CONST 19
JGT L12
!     Out.Int(b[j], 0); Out.Ln
CONST 0
GLOBAL tMaurer.b
LDGW tMaurer.j
CONST 20
BOUND 35
LDIW
GLOBAL Out.Int
CALL 2
GLOBAL Out.Ln
CALL 0
!   FOR j := 0 TO LEN(b)-1 DO
LDGW tMaurer.j
INC
STGW tMaurer.j
JUMP L11
LABEL L12
RETURN
END

! Global variables
GLOVAR tMaurer.j 4
GLOVAR tMaurer.b 80

! End of file
]]*)
