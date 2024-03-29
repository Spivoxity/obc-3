MODULE tChoose;

IMPORT Out;

CONST NMAX = 20;

TYPE Vec = ARRAY NMAX OF BOOLEAN;

PROCEDURE Move(VAR a: Vec; i, j: INTEGER);
BEGIN
  ASSERT(~a[i] & a[j]);
  a[i] := TRUE; a[j] := FALSE
END Move;

PROCEDURE Enumerate(n, k: INTEGER;
                    Visit: PROCEDURE (VAR a: Vec; n: INTEGER));
  VAR i, r, q, s: INTEGER; a: Vec;
BEGIN
  FOR i := 0 TO n-1 DO a[i] := (i < k) END;
  r := k; q := n-k;

  LOOP
    (* Invariant: a[0..r) = 1; a[r..r+q) = 0. *)
    Visit(a, n);

    IF r > 0 THEN
      (* Replace 11..11 10.. by 11..11 01.. *)
      Move(a, r, r-1);
      r := r-1; q := 1
    ELSIF q = n-k THEN
      EXIT
    ELSE
      (* Replace 00...00 11...11 10 with 11...11 00...00 01 *)
      s := 0;
      WHILE a[q+s+1] DO s := s+1 END;
      (* 0^q 1^s 1 0 *)
      IF s <= q THEN
        (* 0^s 0^(q-s) 1^s --> 1^s 0^(q-s) 0^s *)
        FOR i := 0 TO s-1 DO Move(a, i, q+i) END
      ELSE
        (* 0^q 1^(s-q) 1^q --> 1^q 1^(s-q) 0^q *)
        FOR i := 0 TO q-1 DO Move(a, i, s+i) END
      END;
      Move(a, q+s+1, q+s);
      (* 1^s 0^q 0 1 *)
      r := s; q := q+1
    END
  END
END Enumerate;

PROCEDURE Print(VAR a: Vec; n: INTEGER);
  VAR i: INTEGER;
BEGIN
  FOR i := 0 TO n-1 DO
    Out.Int(ORD(a[i]), 0)
  END;
  Out.Ln
END Print;

BEGIN
  Enumerate(8, 4, Print)
END tChoose.
  
(*<<
11110000
11101000
11011000
10111000
01111000
11100100
11010100
10110100
01110100
11001100
10101100
01101100
10011100
01011100
00111100
11100010
11010010
10110010
01110010
11001010
10101010
01101010
10011010
01011010
00111010
11000110
10100110
01100110
10010110
01010110
00110110
10001110
01001110
00101110
00011110
11100001
11010001
10110001
01110001
11001001
10101001
01101001
10011001
01011001
00111001
11000101
10100101
01100101
10010101
01010101
00110101
10001101
01001101
00101101
00011101
11000011
10100011
01100011
10010011
01010011
00110011
10001011
01001011
00101011
00011011
10000111
01000111
00100111
00010111
00001111
>>*)

(*[[
!! (SYMFILE #tChoose STAMP #tChoose.%main 1 #tChoose.m)
!! (CHKSUM STAMP)
!! 
MODULE tChoose STAMP 0
IMPORT Out STAMP
ENDHDR

PROC tChoose.Move 0 4 0x00100001
! PROCEDURE Move(VAR a: Vec; i, j: INTEGER);
!   ASSERT(~a[i] & a[j]);
LDLW 12
LDLW 16
CONST 20
BOUND 11
LDIC
JNEQZ L1
LDLW 12
LDLW 20
CONST 20
BOUND 11
LDIC
JNEQZ L2
LABEL L1
CONST 0
CONST 11
GLOBAL EASSERT
CALL 2
LABEL L2
!   a[i] := TRUE; a[j] := FALSE
CONST 1
LDLW 12
LDLW 16
CONST 20
BOUND 12
STIC
CONST 0
LDLW 12
LDLW 20
CONST 20
BOUND 12
STIC
RETURN
END

PROC tChoose.Enumerate 48 4 0
! PROCEDURE Enumerate(n, k: INTEGER;
!   FOR i := 0 TO n-1 DO a[i] := (i < k) END;
LDLW 12
DEC
STLW -40
CONST 0
STLW -4
LABEL L4
LDLW -4
LDLW -40
JGT L5
LDLW -4
LDLW 16
LT
LOCAL -36
LDLW -4
CONST 20
BOUND 19
STIC
INCL -4
JUMP L4
LABEL L5
!   r := k; q := n-k;
LDLW 16
STLW -8
LDLW 12
LDLW 16
MINUS
STLW -12
LABEL L6
!     Visit(a, n);
LDLW 12
LOCAL -36
LDLW 24
STATLINK
LDLW 20
NCHECK 24
CALL 2
!     IF r > 0 THEN
LDLW -8
JLEQZ L20
!       Move(a, r, r-1);
LDLW -8
DEC
LDLW -8
LOCAL -36
GLOBAL tChoose.Move
CALL 3
!       r := r-1; q := 1
DECL -8
CONST 1
STLW -12
JUMP L6
LABEL L20
!     ELSIF q = n-k THEN
LDLW -12
LDLW 12
LDLW 16
MINUS
JEQ L7
!       s := 0;
CONST 0
STLW -16
LABEL L9
!       WHILE a[q+s+1] DO s := s+1 END;
LOCAL -36
LDLW -12
LDLW -16
PLUS
INC
CONST 20
BOUND 35
LDIC
JEQZ L11
INCL -16
JUMP L9
LABEL L11
!       IF s <= q THEN
LDLW -16
LDLW -12
JGT L16
!         FOR i := 0 TO s-1 DO Move(a, i, q+i) END
LDLW -16
DEC
STLW -44
CONST 0
STLW -4
LABEL L17
LDLW -4
LDLW -44
JGT L14
LDLW -12
LDLW -4
PLUS
LDLW -4
LOCAL -36
GLOBAL tChoose.Move
CALL 3
INCL -4
JUMP L17
LABEL L16
!         FOR i := 0 TO q-1 DO Move(a, i, s+i) END
LDLW -12
DEC
STLW -48
CONST 0
STLW -4
LABEL L13
LDLW -4
LDLW -48
JGT L14
LDLW -16
LDLW -4
PLUS
LDLW -4
LOCAL -36
GLOBAL tChoose.Move
CALL 3
INCL -4
JUMP L13
LABEL L14
!       Move(a, q+s+1, q+s);
LDLW -12
LDLW -16
PLUS
LDLW -12
LDLW -16
PLUS
INC
LOCAL -36
GLOBAL tChoose.Move
CALL 3
!       r := s; q := q+1
LDLW -16
STLW -8
INCL -12
JUMP L6
LABEL L7
RETURN
END

PROC tChoose.Print 8 4 0x00100001
! PROCEDURE Print(VAR a: Vec; n: INTEGER);
!   FOR i := 0 TO n-1 DO
LDLW 16
DEC
STLW -8
CONST 0
STLW -4
LABEL L23
LDLW -4
LDLW -8
JGT L24
!     Out.Int(ORD(a[i]), 0)
CONST 0
LDLW 12
LDLW -4
CONST 20
BOUND 55
LDIC
GLOBAL Out.Int
CALL 2
!   FOR i := 0 TO n-1 DO
INCL -4
JUMP L23
LABEL L24
!   Out.Ln
GLOBAL Out.Ln
CALL 0
RETURN
END

PROC tChoose.%main 0 5 0
!   Enumerate(8, 4, Print)
CONST 0
GLOBAL tChoose.Print
CONST 4
CONST 8
GLOBAL tChoose.Enumerate
CALL 4
RETURN
END

! End of file
]]*)
