MODULE tAlphabet;

IMPORT Random, Out;

PROCEDURE Print(VAR a: ARRAY OF INTEGER; pos, len: INTEGER);
  VAR i: INTEGER;
BEGIN
  FOR i := pos TO pos+len-1 DO
    Out.Char(CHR(ORD('A')+a[i]))
  END
END Print;

PROCEDURE MinLength(VAR a: ARRAY OF INTEGER; K: INTEGER);
  CONST INF = 1000000;
  VAR lo, hi, total, pos, min, i: INTEGER;
    count: ARRAY 100 OF INTEGER;
BEGIN
  lo := 0; hi := 0; total := 0;
  FOR i := 0 TO K-1 DO count[i] := 0 END;
  min := INF; pos := 0;

  WHILE (hi < LEN(a)) OR (total = K) DO
    IF total < K THEN
      IF count[a[hi]] = 0 THEN INC(total) END;
      INC(count[a[hi]]);
      INC(hi)
    ELSE
      IF hi - lo < min THEN
        min := hi - lo; pos := lo
      END;
      DEC(count[a[lo]]);
      IF count[a[lo]] = 0 THEN DEC(total) END;
      INC(lo)
    END
  END;

  IF min < INF THEN
    Print(a, 0, pos);
    Out.Char('['); Print(a, pos, min); Out.Char(']');
    Print(a, pos+min, LEN(a)-pos-min); Out.Ln
  ELSE
    i := 0; WHILE count[i] > 0 DO INC(i) END;
    Out.String("No "); Out.Char(CHR(ORD('A')+i)); Out.String(": ");
    Print(a, 0, LEN(a)); Out.Ln
  END
END MinLength;

PROCEDURE Test(K: INTEGER);
  VAR a: ARRAY 70 OF INTEGER; i: INTEGER;
BEGIN
  FOR i := 0 TO 69 DO a[i] := Random.Roll(K) END;
  MinLength(a, K)
END Test;

BEGIN
  Test(10); Test(10); Test(10);
  Test(10); Test(10); Test(10);
  Test(10); Test(10); Test(10);
  Test(10); Test(10); Test(10);
  Test(26); Test(26); Test(26);
  Test(26); Test(26); Test(26)
END tAlphabet.

(*<<
BBJJH[IDAGBBFJAHGHFAAEC]IGEADHHHFDIEJEAAEDICIIDDBHEEHJBEABCIJAGCABHDEADD
ECBCIEJACJBAAAIFCGAHAHIGECDJDIIHJDABFI[IDCHFBFJADGE]HJDBCBFDAJBAEFFGBBBB
EGAHCEBJJEBJAECGDAEJIBAHGBJBGDHEGCHDHDABCDIBEC[GEAECHDHDIFBAAEDJ]DEFFFGI
CACBDHJCAIFJDIGFBFBGBJFDBCEBHJIJEGIB[CEJHFHBIFDBDGHJBEFA]DGDHJFJCJGHDFDE
GJBAHDEHACJJCFFGAAGIGCHEJJFFAIIHHIGHBHGJHHCAIBJIH[FACIEHABCJHBDG]ADCJGIA
HDHBCAABEAIIEEBADIFJBEEEEGF[JCFIDEBAHG]CDDJFAIBFGAFDBJHGICDIIGBBCADGFGCD
EGBIHCHGFCAEDFIAIEFAEACJCJEBBHCDDHFHCIHE[BIHACEDFFCJG]EEACGAECJBIHEHHHCC
EHIGIJFGDEDDCHDIJJEGDBDABAD[HJDFBEBAJIAICFDCG]CGDCHFJIBFDIGIEDJFDGHBFAGG
BHIFFDCBDJAHIBHADEJECHGAJCGIGGJDCJBBI[HEFBIJBCJAIDAAAG]IEIBCBEFJCFBHHJEF
JDECCAACDGHBEBBCAEJFAIHFAAHE[AIEFBJHIGDGC]DBBJEECCIJDCGEJHEFAIIDIGFBACDE
JJEJCGHJCIFIABBHAAFAAABIEBBBBAEIBDEGJCCDIGBDEEGDJIBH[IECCDDBEEHFGCEAJ]DA
ABABGDCCBBCIEGGEBHHJDJDFECGCFHHFGEEDCGEHEBI[BGAEHJCCFID]JDBHJDIIDCHHIAAF
No C: IHXYAKBNGWZKXNAFLBMKWQRFLHJBSFXOYBZTIWHOYIKLORWMNVRHGPUVOLQBWFYGWQSFYG
No C: PVYYHGPDBEJXMULIQQZYPRBBJGVOSZLMGMSEHFHLWENWEAEEUITBLQBORYDNXZRHNGMFFM
No V: UHKQLYOKPTNNDLBGTMUWAUMYBLKNTCJYJRYMJJSOHFDEERDMBGXMYTXUAIJZZWIAOFCAAZ
MXCQVMEFXGRU[PMWNCDDHSIOOTDGZHGQFUJUMMBVDEQRSSYDKDAEMQKIDVNXSRYCJMTDOL]R
No B: NJSUXJJRIOULHVQISKPPXDIJDGLORVDRUXRGWDMMLCKCMJTSKVDMZUAWZWKZNHNPFXUUJG
No G: ESNYEMVZPFYBNVXTEXLRLMBZSURWIQCXDAURLSVMHFQYPTJJSQEAKFIVPNOHTJHHQVULJW
>>*)

(*[[
!! (SYMFILE #tAlphabet STAMP #tAlphabet.%main 1 #tAlphabet.m)
!! (CHKSUM STAMP)
!! 
MODULE tAlphabet STAMP 0
IMPORT Random STAMP
IMPORT Out STAMP
ENDHDR

PROC tAlphabet.Print 8 3 0x00100001
! PROCEDURE Print(VAR a: ARRAY OF INTEGER; pos, len: INTEGER);
!   FOR i := pos TO pos+len-1 DO
LDLW 20
LDLW 24
PLUS
DEC
STLW -8
LDLW 20
STLW -4
LABEL L3
LDLW -4
LDLW -8
JGT L4
!     Out.Char(CHR(ORD('A')+a[i]))
LDLW 12
LDLW -4
LDLW 16
BOUND 9
LDIW
CONST 65
PLUS
CONVNC
ALIGNC
GLOBAL Out.Char
CALL 1
!   FOR i := pos TO pos+len-1 DO
INCL -4
JUMP L3
LABEL L4
RETURN
END

PROC tAlphabet.MinLength 428 5 0x00100001
! PROCEDURE MinLength(VAR a: ARRAY OF INTEGER; K: INTEGER);
!   lo := 0; hi := 0; total := 0;
CONST 0
STLW -4
CONST 0
STLW -8
CONST 0
STLW -12
!   FOR i := 0 TO K-1 DO count[i] := 0 END;
LDLW 20
DEC
STLW -428
CONST 0
STLW -24
LABEL L5
LDLW -24
LDLW -428
JGT L6
CONST 0
LOCAL -424
LDLW -24
CONST 100
BOUND 19
STIW
INCL -24
JUMP L5
LABEL L6
!   min := INF; pos := 0;
CONST 1000000
STLW -20
CONST 0
STLW -16
LABEL L7
!   WHILE (hi < LEN(a)) OR (total = K) DO
LDLW -8
LDLW 16
JLT L8
LDLW -12
LDLW 20
JNEQ L9
LABEL L8
!     IF total < K THEN
LDLW -12
LDLW 20
JGEQ L18
!       IF count[a[hi]] = 0 THEN INC(total) END;
LOCAL -424
LDLW 12
LDLW -8
LDLW 16
BOUND 24
LDIW
CONST 100
BOUND 24
LDIW
JNEQZ L21
INCL -12
LABEL L21
!       INC(count[a[hi]]);
LOCAL -424
LDLW 12
LDLW -8
LDLW 16
BOUND 25
LDIW
CONST 100
BOUND 25
INDEXW
DUP 0
LOADW
INC
SWAP
STOREW
!       INC(hi)
INCL -8
JUMP L7
LABEL L18
!       IF hi - lo < min THEN
LDLW -8
LDLW -4
MINUS
LDLW -20
JGEQ L13
!         min := hi - lo; pos := lo
LDLW -8
LDLW -4
MINUS
STLW -20
LDLW -4
STLW -16
LABEL L13
!       DEC(count[a[lo]]);
LOCAL -424
LDLW 12
LDLW -4
LDLW 16
BOUND 31
LDIW
CONST 100
BOUND 31
INDEXW
DUP 0
LOADW
DEC
SWAP
STOREW
!       IF count[a[lo]] = 0 THEN DEC(total) END;
LOCAL -424
LDLW 12
LDLW -4
LDLW 16
BOUND 32
LDIW
CONST 100
BOUND 32
LDIW
JNEQZ L16
DECL -12
LABEL L16
!       INC(lo)
INCL -4
JUMP L7
LABEL L9
!   IF min < INF THEN
LDLW -20
CONST 1000000
JGEQ L28
!     Print(a, 0, pos);
LDLW -16
CONST 0
LDLW 16
LDLW 12
GLOBAL tAlphabet.Print
CALL 4
!     Out.Char('['); Print(a, pos, min); Out.Char(']');
CONST 91
ALIGNC
GLOBAL Out.Char
CALL 1
LDLW -20
LDLW -16
LDLW 16
LDLW 12
GLOBAL tAlphabet.Print
CALL 4
CONST 93
ALIGNC
GLOBAL Out.Char
CALL 1
!     Print(a, pos+min, LEN(a)-pos-min); Out.Ln
LDLW 16
LDLW -16
MINUS
LDLW -20
MINUS
LDLW -16
LDLW -20
PLUS
LDLW 16
LDLW 12
GLOBAL tAlphabet.Print
CALL 4
GLOBAL Out.Ln
CALL 0
RETURN
LABEL L28
!     i := 0; WHILE count[i] > 0 DO INC(i) END;
CONST 0
STLW -24
LABEL L24
LOCAL -424
LDLW -24
CONST 100
BOUND 42
LDIW
JLEQZ L26
INCL -24
JUMP L24
LABEL L26
!     Out.String("No "); Out.Char(CHR(ORD('A')+i)); Out.String(": ");
CONST 4
GLOBAL tAlphabet.%1
GLOBAL Out.String
CALL 2
LDLW -24
CONST 65
PLUS
CONVNC
ALIGNC
GLOBAL Out.Char
CALL 1
CONST 3
GLOBAL tAlphabet.%2
GLOBAL Out.String
CALL 2
!     Print(a, 0, LEN(a)); Out.Ln
LDLW 16
CONST 0
LDLW 16
LDLW 12
GLOBAL tAlphabet.Print
CALL 4
GLOBAL Out.Ln
CALL 0
RETURN
END

PROC tAlphabet.Test 284 4 0
! PROCEDURE Test(K: INTEGER);
!   FOR i := 0 TO 69 DO a[i] := Random.Roll(K) END;
CONST 0
STLW -284
LABEL L29
LDLW -284
CONST 69
JGT L30
LDLW 12
GLOBAL Random.Roll
CALLW 1
LOCAL -280
LDLW -284
CONST 70
BOUND 51
STIW
INCL -284
JUMP L29
LABEL L30
!   MinLength(a, K)
LDLW 12
CONST 70
LOCAL -280
GLOBAL tAlphabet.MinLength
CALL 3
RETURN
END

PROC tAlphabet.%main 0 2 0
!   Test(10); Test(10); Test(10);
CONST 10
GLOBAL tAlphabet.Test
CALL 1
CONST 10
GLOBAL tAlphabet.Test
CALL 1
CONST 10
GLOBAL tAlphabet.Test
CALL 1
!   Test(10); Test(10); Test(10);
CONST 10
GLOBAL tAlphabet.Test
CALL 1
CONST 10
GLOBAL tAlphabet.Test
CALL 1
CONST 10
GLOBAL tAlphabet.Test
CALL 1
!   Test(10); Test(10); Test(10);
CONST 10
GLOBAL tAlphabet.Test
CALL 1
CONST 10
GLOBAL tAlphabet.Test
CALL 1
CONST 10
GLOBAL tAlphabet.Test
CALL 1
!   Test(10); Test(10); Test(10);
CONST 10
GLOBAL tAlphabet.Test
CALL 1
CONST 10
GLOBAL tAlphabet.Test
CALL 1
CONST 10
GLOBAL tAlphabet.Test
CALL 1
!   Test(26); Test(26); Test(26);
CONST 26
GLOBAL tAlphabet.Test
CALL 1
CONST 26
GLOBAL tAlphabet.Test
CALL 1
CONST 26
GLOBAL tAlphabet.Test
CALL 1
!   Test(26); Test(26); Test(26)
CONST 26
GLOBAL tAlphabet.Test
CALL 1
CONST 26
GLOBAL tAlphabet.Test
CALL 1
CONST 26
GLOBAL tAlphabet.Test
CALL 1
RETURN
END

! String "No "
DEFINE tAlphabet.%1
STRING 4E6F2000

! String ": "
DEFINE tAlphabet.%2
STRING 3A2000

! End of file
]]*)
