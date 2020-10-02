MODULE tBrack;

IMPORT Out;

(* Dynamic programming example from DSA *)

(*<<
A
  A = A (1 ways) OK
B
  B = B (1 ways) OK
C
  C = C (1 ways) OK
AC
  A = (AC) (1 ways) OK
BA
  C = (BA) (1 ways) OK
ABA
  A = (A(BA)) (1 ways) OK
  C = ((AB)A) (1 ways) OK
BBA
  A = (B(BA)) (1 ways) OK
  C = ((BB)A) (1 ways) OK
ABBA
  A = (A((BB)A)) (2 ways) OK
  B = (A(B(BA))) (1 ways) OK
  C = ((A(BB))A) (2 ways) OK
ABACAB
  A = (A(B(A(C(AB))))) (6 ways) OK
  B = (A(B(A((CA)B)))) (31 ways) OK
  C = ((AB)(A(C(AB)))) (5 ways) OK
ABBCACB
  A = (A(B(B(C((AC)B))))) (30 ways) OK
  B = (A(B(B(C(A(CB)))))) (82 ways) OK
  C = ((AB)(B(C((AC)B)))) (20 ways) OK
BBCABCA
  A = (B(B(C(A(B(CA)))))) (44 ways) OK
  B = (B(B((CA)((BC)A)))) (46 ways) OK
  C = (B(B(C(A((BC)A))))) (42 ways) OK
ABBCABC
  A = (A(B(B(C(A(BC)))))) (64 ways) OK
  B = (A(B(B(C((AB)C))))) (42 ways) OK
  C = ((AB)(B(C(A(BC))))) (26 ways) OK
BBBBBBBBBB
  B = (B(B(B(B(B(B(B(B(BB))))))))) (4862 ways) OK
ABBBBBBBBBBBBC
  A = (A(B(B(B(B(B(B(B(B(B(B(B(BC))))))))))))) (417022 ways) OK
  B = (A(B(B(B(B(B(B(B(B(B(B((BB)C)))))))))))) (116868 ways) OK
  C = ((AB)(B(B(B(B(B(B(B(B(B(B(BC)))))))))))) (209010 ways) OK
ABBBBBBBBBBBBBBBBC
  A = (A(B(B(B(B(B(B(B(B(B(B(B(B(B(B(B(BC))))))))))))))))) (72596742 ways) OK
  B = (A(B(B(B(B(B(B(B(B(B(B(B(B(B(B((BB)C)))))))))))))))) (19808976 ways) OK
  C = ((AB)(B(B(B(B(B(B(B(B(B(B(B(B(B(B(BC)))))))))))))))) (37239072 ways) OK
>>*)

CONST MAX = 30;

CONST A = 0; B = 1; C = 2; STAR = 3;

VAR 
  op: ARRAY 3 OF ARRAY 3 OF INTEGER;

TYPE
  Exp = POINTER TO Blob;
  Blob = RECORD rator: INTEGER; left, right: Exp END;

(* For 0 <= i < j <= n, ways[i,j][x] is the number of ways of making
   x with s[i..j), and exp[i,j][x] is an example expression. *)

VAR
  N: INTEGER;
  ways: ARRAY MAX, MAX, 3 OF INTEGER;
  exp: ARRAY MAX, MAX, 3 OF Exp;

PROCEDURE MakeExp(rator: INTEGER; left, right: Exp): Exp;
  VAR p: Exp;
BEGIN
  NEW(p);
  p.rator := rator; p.left := left; p.right := right;
  RETURN p
END MakeExp;

PROCEDURE Reset(a, b: INTEGER);
  VAR x: INTEGER;
BEGIN
  FOR x := 0 TO 2 DO 
    ways[a,b][x] := 0; exp[a,b][x] := NIL
  END
END Reset;

PROCEDURE Init(VAR s: ARRAY OF CHAR);
  VAR n, x: INTEGER;
BEGIN
  n := 0;
  WHILE s[n] # 0X DO
    Reset(n, n+1);
    x := ORD(s[n]) - ORD('A');
    ways[n,n+1][x] := 1; 
    exp[n,n+1][x] := MakeExp(x, NIL, NIL);
    n := n+1
  END;

  N := n
END Init;    

PROCEDURE Combine(i, j, k: INTEGER);
  VAR x, y, z: INTEGER;
BEGIN
  (* Combine ways[i,j] with ways[j,k] and add to ways[i,k] *)
  FOR x := 0 TO 2 DO
    IF ways[i,j][x] > 0 THEN
      FOR y := 0 TO 2 DO
        IF ways[j,k][y] > 0 THEN
	  z := op[x,y];
	  INC(ways[i,k][z], ways[i,j][x] * ways[j,k][y]);
	  IF exp[i,k][z] = NIL THEN
	    exp[i,k][z] := MakeExp(STAR, exp[i,j][x], exp[j,k][y])
	  END
	END
      END
    END
  END
END Combine;

PROCEDURE Tabulate(s: ARRAY OF CHAR);
  VAR i, j, k, u: INTEGER;
BEGIN
  Init(s);
  FOR k := 2 TO N DO
    FOR i := 0 TO N-k DO
      j := i+k;
      Reset(i, j);
      FOR u := i+1 TO j-1 DO
	Combine(i, u, j)
      END
    END
  END
END Tabulate;

PROCEDURE Eval(e: Exp): INTEGER;
BEGIN
  IF e.rator = STAR THEN
    RETURN op[Eval(e.left), Eval(e.right)]
  ELSE
    RETURN e.rator
  END
END Eval;

PROCEDURE Print(e: Exp);
BEGIN
  IF e.rator = STAR THEN
    Out.Char('('); Print(e.left); Print(e.right); Out.Char(')')
  ELSE
    Out.Char(CHR(e.rator + ORD('A')))
  END
END Print;

PROCEDURE Test(s: ARRAY OF CHAR);
  VAR x: INTEGER;
BEGIN
  Out.String(s); Out.Ln;
  Tabulate(s);
  FOR x := 0 TO 2 DO
    IF ways[0,N][x] > 0 THEN
      Out.String("  "); Out.Char(CHR(x + ORD('A'))); Out.String(" = ");
      Print(exp[0,N][x]); Out.String(" ("); Out.Int(ways[0,N][x], 0); 
      Out.String(" ways)"); 

      IF Eval(exp[0,N][x]) = x THEN
        Out.String(" OK")
      ELSE
        Out.String(" OOOOOOPS!")
      END;

      Out.Ln
    END
  END
END Test;

BEGIN
  op[A,A] := B; op[A,B] := B; op[A,C] := A;
  op[B,A] := C; op[B,B] := B; op[B,C] := A;
  op[C,A] := A; op[C,B] := C; op[C,C] := C;

  Test("A");
  Test("B");
  Test("C");
  Test("AC");
  Test("BA");
  Test("ABA");
  Test("BBA");
  Test("ABBA");
  Test("ABACAB");
  Test("ABBCACB");
  Test("BBCABCA");
  Test("ABBCABC");
  Test("BBBBBBBBBB");
  Test("ABBBBBBBBBBBBC");
  Test("ABBBBBBBBBBBBBBBBC");
END tBrack.

(*[[
!! (SYMFILE #tBrack STAMP #tBrack.%main 1 #tBrack.m)
!! (CHKSUM STAMP)
!! 
MODULE tBrack STAMP 0
IMPORT Out STAMP
ENDHDR

PROC tBrack.MakeExp 4 3 0x00610001
! PROCEDURE MakeExp(rator: INTEGER; left, right: Exp): Exp;
!   NEW(p);
CONST 12
GLOBAL tBrack.Blob
GLOBAL NEW
CALLW 2
STLW -4
!   p.rator := rator; p.left := left; p.right := right;
LDLW 12
LDLW -4
NCHECK 79
STOREW
LDLW 16
LDLW -4
NCHECK 79
STNW 4
LDLW 20
LDLW -4
NCHECK 79
STNW 8
!   RETURN p
LDLW -4
RETURN
END

PROC tBrack.Reset 4 5 0
! PROCEDURE Reset(a, b: INTEGER);
!   FOR x := 0 TO 2 DO 
CONST 0
STLW -4
LABEL L22
LDLW -4
CONST 2
JGT L23
!     ways[a,b][x] := 0; exp[a,b][x] := NIL
CONST 0
GLOBAL tBrack.ways
LDLW 12
CONST 30
BOUND 87
CONST 30
TIMES
LDLW 16
CONST 30
BOUND 87
PLUS
CONST 3
TIMES
LDLW -4
CONST 3
BOUND 87
PLUS
STIW
CONST 0
GLOBAL tBrack.exp
LDLW 12
CONST 30
BOUND 87
CONST 30
TIMES
LDLW 16
CONST 30
BOUND 87
PLUS
CONST 3
TIMES
LDLW -4
CONST 3
BOUND 87
PLUS
STIW
!   FOR x := 0 TO 2 DO 
INCL -4
JUMP L22
LABEL L23
RETURN
END

PROC tBrack.Init 8 5 0x00100001
! PROCEDURE Init(VAR s: ARRAY OF CHAR);
!   n := 0;
CONST 0
STLW -4
LABEL L24
!   WHILE s[n] # 0X DO
LDLW 12
LDLW -4
LDLW 16
BOUND 95
LDIC
JEQZ L26
!     Reset(n, n+1);
LDLW -4
INC
LDLW -4
GLOBAL tBrack.Reset
CALL 2
!     x := ORD(s[n]) - ORD('A');
LDLW 12
LDLW -4
LDLW 16
BOUND 97
LDIC
CONST 65
MINUS
STLW -8
!     ways[n,n+1][x] := 1; 
CONST 1
GLOBAL tBrack.ways
LDLW -4
CONST 30
BOUND 98
CONST 30
TIMES
LDLW -4
INC
CONST 30
BOUND 98
PLUS
CONST 3
TIMES
LDLW -8
CONST 3
BOUND 98
PLUS
STIW
!     exp[n,n+1][x] := MakeExp(x, NIL, NIL);
CONST 0
CONST 0
LDLW -8
GLOBAL tBrack.MakeExp
CALLW 3
GLOBAL tBrack.exp
LDLW -4
CONST 30
BOUND 99
CONST 30
TIMES
LDLW -4
INC
CONST 30
BOUND 99
PLUS
CONST 3
TIMES
LDLW -8
CONST 3
BOUND 99
PLUS
STIW
!     n := n+1
INCL -4
JUMP L24
LABEL L26
!   N := n
LDLW -4
STGW tBrack.N
RETURN
END

PROC tBrack.Combine 12 7 0
! PROCEDURE Combine(i, j, k: INTEGER);
!   FOR x := 0 TO 2 DO
CONST 0
STLW -4
LABEL L27
LDLW -4
CONST 2
JGT L28
!     IF ways[i,j][x] > 0 THEN
GLOBAL tBrack.ways
LDLW 12
CONST 30
BOUND 111
CONST 30
TIMES
LDLW 16
CONST 30
BOUND 111
PLUS
CONST 3
TIMES
LDLW -4
CONST 3
BOUND 111
PLUS
LDIW
JLEQZ L31
!       FOR y := 0 TO 2 DO
CONST 0
STLW -8
LABEL L32
LDLW -8
CONST 2
JGT L31
!         IF ways[j,k][y] > 0 THEN
GLOBAL tBrack.ways
LDLW 16
CONST 30
BOUND 113
CONST 30
TIMES
LDLW 20
CONST 30
BOUND 113
PLUS
CONST 3
TIMES
LDLW -8
CONST 3
BOUND 113
PLUS
LDIW
JLEQZ L36
! 	  z := op[x,y];
GLOBAL tBrack.op
LDLW -4
CONST 3
BOUND 114
CONST 3
TIMES
LDLW -8
CONST 3
BOUND 114
PLUS
LDIW
STLW -12
! 	  INC(ways[i,k][z], ways[i,j][x] * ways[j,k][y]);
GLOBAL tBrack.ways
LDLW 12
CONST 30
BOUND 115
CONST 30
TIMES
LDLW 20
CONST 30
BOUND 115
PLUS
CONST 3
TIMES
LDLW -12
CONST 3
BOUND 115
PLUS
INDEXW
DUP 0
LOADW
GLOBAL tBrack.ways
LDLW 12
CONST 30
BOUND 115
CONST 30
TIMES
LDLW 16
CONST 30
BOUND 115
PLUS
CONST 3
TIMES
LDLW -4
CONST 3
BOUND 115
PLUS
LDIW
GLOBAL tBrack.ways
LDLW 16
CONST 30
BOUND 115
CONST 30
TIMES
LDLW 20
CONST 30
BOUND 115
PLUS
CONST 3
TIMES
LDLW -8
CONST 3
BOUND 115
PLUS
LDIW
TIMES
PLUS
SWAP
STOREW
! 	  IF exp[i,k][z] = NIL THEN
GLOBAL tBrack.exp
LDLW 12
CONST 30
BOUND 116
CONST 30
TIMES
LDLW 20
CONST 30
BOUND 116
PLUS
CONST 3
TIMES
LDLW -12
CONST 3
BOUND 116
PLUS
LDIW
JNEQZ L36
! 	    exp[i,k][z] := MakeExp(STAR, exp[i,j][x], exp[j,k][y])
GLOBAL tBrack.exp
LDLW 16
CONST 30
BOUND 117
CONST 30
TIMES
LDLW 20
CONST 30
BOUND 117
PLUS
CONST 3
TIMES
LDLW -8
CONST 3
BOUND 117
PLUS
LDIW
GLOBAL tBrack.exp
LDLW 12
CONST 30
BOUND 117
CONST 30
TIMES
LDLW 16
CONST 30
BOUND 117
PLUS
CONST 3
TIMES
LDLW -4
CONST 3
BOUND 117
PLUS
LDIW
CONST 3
GLOBAL tBrack.MakeExp
CALLW 3
GLOBAL tBrack.exp
LDLW 12
CONST 30
BOUND 117
CONST 30
TIMES
LDLW 20
CONST 30
BOUND 117
PLUS
CONST 3
TIMES
LDLW -12
CONST 3
BOUND 117
PLUS
STIW
LABEL L36
!       FOR y := 0 TO 2 DO
INCL -8
JUMP L32
LABEL L31
!   FOR x := 0 TO 2 DO
INCL -4
JUMP L27
LABEL L28
RETURN
END

PROC tBrack.Tabulate 28 4 0
! PROCEDURE Tabulate(s: ARRAY OF CHAR);
LOCAL 12
LDLW 16
FLEXCOPY
!   Init(s);
LDLW 16
LDLW 12
GLOBAL tBrack.Init
CALL 2
!   FOR k := 2 TO N DO
LDGW tBrack.N
STLW -20
CONST 2
STLW -12
LABEL L40
LDLW -12
LDLW -20
JGT L41
!     FOR i := 0 TO N-k DO
LDGW tBrack.N
LDLW -12
MINUS
STLW -24
CONST 0
STLW -4
LABEL L42
LDLW -4
LDLW -24
JGT L43
!       j := i+k;
LDLW -4
LDLW -12
PLUS
STLW -8
!       Reset(i, j);
LDLW -8
LDLW -4
GLOBAL tBrack.Reset
CALL 2
!       FOR u := i+1 TO j-1 DO
LDLW -8
DEC
STLW -28
LDLW -4
INC
STLW -16
LABEL L44
LDLW -16
LDLW -28
JGT L45
! 	Combine(i, u, j)
LDLW -8
LDLW -16
LDLW -4
GLOBAL tBrack.Combine
CALL 3
!       FOR u := i+1 TO j-1 DO
INCL -16
JUMP L44
LABEL L45
!     FOR i := 0 TO N-k DO
INCL -4
JUMP L42
LABEL L43
!   FOR k := 2 TO N DO
INCL -12
JUMP L40
LABEL L41
RETURN
END

PROC tBrack.Eval 0 4 0x00100001
! PROCEDURE Eval(e: Exp): INTEGER;
!   IF e.rator = STAR THEN
LDLW 12
NCHECK 142
LOADW
CONST 3
JNEQ L48
!     RETURN op[Eval(e.left), Eval(e.right)]
GLOBAL tBrack.op
LDLW 12
NCHECK 143
LDNW 4
GLOBAL tBrack.Eval
CALLW 1
CONST 3
BOUND 143
CONST 3
TIMES
LDLW 12
NCHECK 143
LDNW 8
GLOBAL tBrack.Eval
CALLW 1
CONST 3
BOUND 143
PLUS
LDIW
RETURN
LABEL L48
!     RETURN e.rator
LDLW 12
NCHECK 145
LOADW
RETURN
END

PROC tBrack.Print 0 2 0x00100001
! PROCEDURE Print(e: Exp);
!   IF e.rator = STAR THEN
LDLW 12
NCHECK 151
LOADW
CONST 3
JNEQ L51
!     Out.Char('('); Print(e.left); Print(e.right); Out.Char(')')
CONST 40
ALIGNC
GLOBAL Out.Char
CALL 1
LDLW 12
NCHECK 152
LDNW 4
GLOBAL tBrack.Print
CALL 1
LDLW 12
NCHECK 152
LDNW 8
GLOBAL tBrack.Print
CALL 1
CONST 41
ALIGNC
GLOBAL Out.Char
CALL 1
RETURN
LABEL L51
!     Out.Char(CHR(e.rator + ORD('A')))
LDLW 12
NCHECK 154
LOADW
CONST 65
PLUS
CONVNC
ALIGNC
GLOBAL Out.Char
CALL 1
RETURN
END

PROC tBrack.Test 4 5 0
! PROCEDURE Test(s: ARRAY OF CHAR);
LOCAL 12
LDLW 16
FLEXCOPY
!   Out.String(s); Out.Ln;
LDLW 16
LDLW 12
GLOBAL Out.String
CALL 2
GLOBAL Out.Ln
CALL 0
!   Tabulate(s);
LDLW 16
LDLW 12
GLOBAL tBrack.Tabulate
CALL 2
!   FOR x := 0 TO 2 DO
CONST 0
STLW -4
LABEL L52
LDLW -4
CONST 2
JGT L53
!     IF ways[0,N][x] > 0 THEN
GLOBAL tBrack.ways
CONST 0
LDGW tBrack.N
CONST 30
BOUND 164
PLUS
CONST 3
TIMES
LDLW -4
CONST 3
BOUND 164
PLUS
LDIW
JLEQZ L56
!       Out.String("  "); Out.Char(CHR(x + ORD('A'))); Out.String(" = ");
CONST 3
GLOBAL tBrack.%1
GLOBAL Out.String
CALL 2
LDLW -4
CONST 65
PLUS
CONVNC
ALIGNC
GLOBAL Out.Char
CALL 1
CONST 4
GLOBAL tBrack.%2
GLOBAL Out.String
CALL 2
!       Print(exp[0,N][x]); Out.String(" ("); Out.Int(ways[0,N][x], 0); 
GLOBAL tBrack.exp
CONST 0
LDGW tBrack.N
CONST 30
BOUND 166
PLUS
CONST 3
TIMES
LDLW -4
CONST 3
BOUND 166
PLUS
LDIW
GLOBAL tBrack.Print
CALL 1
CONST 3
GLOBAL tBrack.%3
GLOBAL Out.String
CALL 2
CONST 0
GLOBAL tBrack.ways
CONST 0
LDGW tBrack.N
CONST 30
BOUND 166
PLUS
CONST 3
TIMES
LDLW -4
CONST 3
BOUND 166
PLUS
LDIW
GLOBAL Out.Int
CALL 2
!       Out.String(" ways)"); 
CONST 7
GLOBAL tBrack.%4
GLOBAL Out.String
CALL 2
!       IF Eval(exp[0,N][x]) = x THEN
GLOBAL tBrack.exp
CONST 0
LDGW tBrack.N
CONST 30
BOUND 169
PLUS
CONST 3
TIMES
LDLW -4
CONST 3
BOUND 169
PLUS
LDIW
GLOBAL tBrack.Eval
CALLW 1
LDLW -4
JNEQ L59
!         Out.String(" OK")
CONST 4
GLOBAL tBrack.%5
GLOBAL Out.String
CALL 2
JUMP L57
LABEL L59
!         Out.String(" OOOOOOPS!")
CONST 11
GLOBAL tBrack.%6
GLOBAL Out.String
CALL 2
LABEL L57
!       Out.Ln
GLOBAL Out.Ln
CALL 0
LABEL L56
!   FOR x := 0 TO 2 DO
INCL -4
JUMP L52
LABEL L53
RETURN
END

PROC tBrack.%main 0 5 0
!   op[A,A] := B; op[A,B] := B; op[A,C] := A;
CONST 1
STGW tBrack.op
CONST 1
GLOBAL tBrack.op
STNW 4
CONST 0
GLOBAL tBrack.op
STNW 8
!   op[B,A] := C; op[B,B] := B; op[B,C] := A;
CONST 2
GLOBAL tBrack.op
STNW 12
CONST 1
GLOBAL tBrack.op
STNW 16
CONST 0
GLOBAL tBrack.op
STNW 20
!   op[C,A] := A; op[C,B] := C; op[C,C] := C;
CONST 0
GLOBAL tBrack.op
STNW 24
CONST 2
GLOBAL tBrack.op
STNW 28
CONST 2
GLOBAL tBrack.op
STNW 32
!   Test("A");
CONST 2
GLOBAL tBrack.%19
GLOBAL tBrack.Test
CALL 2
!   Test("B");
CONST 2
GLOBAL tBrack.%20
GLOBAL tBrack.Test
CALL 2
!   Test("C");
CONST 2
GLOBAL tBrack.%21
GLOBAL tBrack.Test
CALL 2
!   Test("AC");
CONST 3
GLOBAL tBrack.%7
GLOBAL tBrack.Test
CALL 2
!   Test("BA");
CONST 3
GLOBAL tBrack.%8
GLOBAL tBrack.Test
CALL 2
!   Test("ABA");
CONST 4
GLOBAL tBrack.%9
GLOBAL tBrack.Test
CALL 2
!   Test("BBA");
CONST 4
GLOBAL tBrack.%10
GLOBAL tBrack.Test
CALL 2
!   Test("ABBA");
CONST 5
GLOBAL tBrack.%11
GLOBAL tBrack.Test
CALL 2
!   Test("ABACAB");
CONST 7
GLOBAL tBrack.%12
GLOBAL tBrack.Test
CALL 2
!   Test("ABBCACB");
CONST 8
GLOBAL tBrack.%13
GLOBAL tBrack.Test
CALL 2
!   Test("BBCABCA");
CONST 8
GLOBAL tBrack.%14
GLOBAL tBrack.Test
CALL 2
!   Test("ABBCABC");
CONST 8
GLOBAL tBrack.%15
GLOBAL tBrack.Test
CALL 2
!   Test("BBBBBBBBBB");
CONST 11
GLOBAL tBrack.%16
GLOBAL tBrack.Test
CALL 2
!   Test("ABBBBBBBBBBBBC");
CONST 15
GLOBAL tBrack.%17
GLOBAL tBrack.Test
CALL 2
!   Test("ABBBBBBBBBBBBBBBBC");
CONST 19
GLOBAL tBrack.%18
GLOBAL tBrack.Test
CALL 2
RETURN
END

! Global variables
GLOVAR tBrack.op 36
GLOVAR tBrack.N 4
GLOVAR tBrack.ways 10800
GLOVAR tBrack.exp 10800

! Global pointer map
DEFINE tBrack.%gcmap
WORD GC_BASE
WORD tBrack.exp
WORD GC_BLOCK
WORD 0
WORD 2700
WORD GC_END

! String "  "
DEFINE tBrack.%1
STRING 202000

! String " = "
DEFINE tBrack.%2
STRING 203D2000

! String " ("
DEFINE tBrack.%3
STRING 202800

! String " ways)"
DEFINE tBrack.%4
STRING 20776179732900

! String " OK"
DEFINE tBrack.%5
STRING 204F4B00

! String " OOOOOOPS!"
DEFINE tBrack.%6
STRING 204F4F4F4F4F4F50532100

! String "AC"
DEFINE tBrack.%7
STRING 414300

! String "BA"
DEFINE tBrack.%8
STRING 424100

! String "ABA"
DEFINE tBrack.%9
STRING 41424100

! String "BBA"
DEFINE tBrack.%10
STRING 42424100

! String "ABBA"
DEFINE tBrack.%11
STRING 4142424100

! String "ABACAB"
DEFINE tBrack.%12
STRING 41424143414200

! String "ABBCACB"
DEFINE tBrack.%13
STRING 4142424341434200

! String "BBCABCA"
DEFINE tBrack.%14
STRING 4242434142434100

! String "ABBCABC"
DEFINE tBrack.%15
STRING 4142424341424300

! String "BBBBBBBBBB"
DEFINE tBrack.%16
STRING 4242424242424242424200

! String "ABBBBBBBBBBBBC"
DEFINE tBrack.%17
STRING 414242424242424242424242424300

! String "ABBBBBBBBBBBBBBBBC"
DEFINE tBrack.%18
STRING 41424242424242424242424242424242424300

! String "A"
DEFINE tBrack.%19
STRING 4100

! String "B"
DEFINE tBrack.%20
STRING 4200

! String "C"
DEFINE tBrack.%21
STRING 4300

! Descriptor for Blob
DEFINE tBrack.Blob
WORD 0x0000000d
WORD 0
WORD tBrack.Blob.%anc

DEFINE tBrack.Blob.%anc
WORD tBrack.Blob

! End of file
]]*)
