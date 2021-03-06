MODULE tRand07;

(* Check the first few random numbers are as expected *)

IMPORT Random, Out;

(* An implementation of the same generator as in Random.m, but using
   bignums, radix B, with N digits. *)

CONST 
  B = 10000; N = 4;
  a = 48271;

TYPE num = ARRAY N OF INTEGER;

VAR u, m: num;

PROCEDURE Greater(VAR a, b: num): BOOLEAN;
  VAR i: INTEGER;
BEGIN
  i := N-1;
  WHILE (i >= 0) & (a[i] = b[i]) DO i := i-1 END;
  RETURN (i >= 0) & (a[i] > b[i])
END Greater;

PROCEDURE Combine(x: INTEGER; VAR a: num; y: INTEGER; VAR b: num);
  VAR i, t, c: INTEGER;
BEGIN
  (* a := xa + yb *)
  c := 0;
  FOR i := 0 TO N-1 DO
    t := x * a[i] + y * b[i] + c; c := t DIV B;
    IF c*B > t THEN Out.Int(c, 12); Out.Int(t, 12); Out.Ln; 
      ASSERT(c*B <= t) END;
    a[i] := t - c*B
  END;
  ASSERT(c = 0)
END Combine;

PROCEDURE next(): INTEGER;
  VAR w: INTEGER;
BEGIN
  (* u := a * u *)
  Combine(a, u, 0, m);

  (* u := u MOD m *)
  WHILE ~ Greater(m, u) DO
    (* Set w so 1 <= w <= u DIV m *)
    IF (u[3] > 0) OR (u[2] > m[2]) THEN
      (* If u' = u DIV B^2 = u3*B + u2 and m' = m DIV B^2 + 1 = m2 + 1,
         then m < m'*B^2 <= u'*B^2 <= u, so 1 <= u' DIV m' <= u DIV m,
	 and the w computed below does not overestimate (u DIV m).

	 On the other hand, to see that w is not unacceptably small, we use
	 the facts (a DIV b) DIV c = a DIV (b * c) and (2*a) DIV b <= 
	 2*(a DIV b) + 1.  Observe that m >= B^2 so m' * B^2 = 
	 (m DIV B^2 + 1) * B^2 <= 2 * m.  Now we calculate
	 
	 u' DIV m' = (u DIV B^2) DIV m' 
		= u DIV (m' * B^2) >= u DIV (2 * m)

	 Thus

	 u DIV m = (2*u) DIV (2*m) 
		<= 2*(u DIV (2*m)) + 1 <= 2*(u' DIV m') + 1 *)

	w := (B*u[3] + u[2]) DIV (m[2] + 1)
    ELSE
	(* u DIV m = 1 *)
        w := 1
    END;

    (* Ensure w <= B to avoid overflow *)
    (* We know u = a*u0 < a*m so u DIV m <= a, and reducing u by B*m each
       time will require about a/B = 4 iterations at most *)
    IF w > B THEN w := B END;

    (* u := u - w*m *)
    Combine(1, u, -w, m);
  END;

  RETURN u[2]*B*B + u[1]*B + u[0]
END next;

VAR x, y, z: INTEGER; r: REAL;

BEGIN
  u[3] := 0; u[2] := 0; u[1] := 3141; u[0] := 5926;
  m[3] := 0; m[2] := 21; m[1] := 4748; m[0] := 3647; 

  (* Check that Random.Random agrees with our slow implementation *)
  FOR x := 1 TO 100 DO 
    y := Random.Random();
    z := next();
    Out.Int(y, 10); Out.Int(z, 12); Out.Ln;
    ASSERT(y = z)
  END;
  Out.Ln;

  (* A more thorough check *)
  FOR x := 1 TO 100000 DO ASSERT(Random.Random() = next()) END;

  (* Check results of Random.Uniform *)
  FOR x := 1 TO 20 DO
    r := Random.Uniform();
    Out.Real(r, 0); Out.Ln;
    ASSERT((0.0 <= r) & (r < 1.0))
  END;
  Out.Ln;

  (* Check results of Random.Roll *)
  FOR x := 1 TO 20 DO
    y := Random.Roll(100000);
    Out.Int(y, 0); Out.Ln;
    ASSERT((0 <= y) & (y < 100000))
  END
END tRand07.

(*<<
 354709164   354709164
 278937913   278937913
2037015380  2037015380
1935662791  1935662791
1512587038  1512587038
1792396945  1792396945
 824278112   824278112
 151732736   151732736
1371663186  1371663186
 337847102   337847102
 226645324   226645324
1114736986  1114736986
2118791974  2118791974
 151204932   151204932
1663840066  1663840066
1482911733  1482911733
1707341839  1707341839
1117989450  1117989450
 204691840   204691840
 107548793   107548793
1019812104  1019812104
 582432003   582432003
1866793936  1866793936
1448772889  1448772889
 911160364   911160364
   9356437     9356437
 673004557   673004557
1617842778  1617842778
1645913683  1645913683
1594387681  1594387681
1168808365  1168808365
 858212931   858212931
1836841671  1836841671
 879483505   879483505
2091535959  2091535959
 883580478   883580478
 140540471   140540471
 128234768   128234768
 972615474   972615474
 834054740   834054740
1780424231  1780424231
 562501661   562501661
1881929110  1881929110
1894317063  1894317063
 725258813   725258813
 689748929   689748929
 284088671   284088671
1561151746  1561151746
1007274289  1007274289
 959952592   959952592
1616917113  1616917113
2060295055  2060295055
 387423688   387423688
1041245372  1041245372
 100593777   100593777
 301683700   301683700
 487272393   487272393
1884780559  1884780559
2097658334  2097658334
  64000817    64000817
1301953021  1301953021
 465347236   465347236
  97481336    97481336
 384899479   384899479
1601720612  1601720612
 801918911   801918911
1035015706  1035015706
  36096871    36096871
 822822324   822822324
 746350539   746350539
 901205997   901205997
 538443908   538443908
 231303427   231303427
 480243964   480243964
1917900526  1917900526
 956268376   956268376
2017269278  2017269278
 106828770   106828770
 623320223   623320223
2044589963  2044589963
 348655147   348655147
 103259298   103259298
 120029071   120029071
  12406635    12406635
1880224219  1880224219
1201902188  1201902188
 602309596   602309596
1452895430  1452895430
 194357804   194357804
1636986788  1636986788
 180968536   180968536
1716208907  1716208907
1790983125  1790983125
1297249596  1297249596
1059585643  1059585643
 640552654   640552654
 647611728   647611728
2093756556  2093756556
 699835915   699835915
1861685655  1861685655

0.226738
0.864667
0.354230
0.0490013
0.343383
0.464189
0.866463
0.0525061
0.521613
0.794844
0.932708
0.759232
0.900341
0.359484
0.670408
0.266190
0.281766
0.140336
0.168203
0.307730

42085
12368
41309
31702
21728
40110
84853
64529
97495
88840
95675
75056
34716
18121
51672
81322
7469
41220
49226
16046
>>*)

(*[[
!! (SYMFILE #tRand07 STAMP #tRand07.%main 1 #tRand07.m)
!! (CHKSUM STAMP)
!! 
MODULE tRand07 STAMP 0
IMPORT Random STAMP
IMPORT Out STAMP
ENDHDR

PROC tRand07.Greater 4 4 0x00300001
! PROCEDURE Greater(VAR a, b: num): BOOLEAN;
!   i := N-1;
CONST 3
STLW -4
LABEL L4
!   WHILE (i >= 0) & (a[i] = b[i]) DO i := i-1 END;
LDLW -4
JLTZ L6
LDLW 12
LDLW -4
CONST 4
BOUND 22
LDIW
LDLW 16
LDLW -4
CONST 4
BOUND 22
LDIW
JNEQ L6
DECL -4
JUMP L4
LABEL L6
!   RETURN (i >= 0) & (a[i] > b[i])
LDLW -4
JGEQZ L2
CONST 0
RETURN
LABEL L2
LDLW 12
LDLW -4
CONST 4
BOUND 23
LDIW
LDLW 16
LDLW -4
CONST 4
BOUND 23
LDIW
GT
RETURN
END

PROC tRand07.Combine 12 5 0x00a00001
! PROCEDURE Combine(x: INTEGER; VAR a: num; y: INTEGER; VAR b: num);
!   c := 0;
CONST 0
STLW -12
!   FOR i := 0 TO N-1 DO
CONST 0
STLW -4
LABEL L8
LDLW -4
CONST 3
JGT L9
!     t := x * a[i] + y * b[i] + c; c := t DIV B;
LDLW 12
LDLW 16
LDLW -4
CONST 4
BOUND 32
LDIW
TIMES
LDLW 20
LDLW 24
LDLW -4
CONST 4
BOUND 32
LDIW
TIMES
PLUS
LDLW -12
PLUS
STLW -8
LDLW -8
CONST 10000
DIV
STLW -12
!     IF c*B > t THEN Out.Int(c, 12); Out.Int(t, 12); Out.Ln; 
LDLW -12
CONST 10000
TIMES
LDLW -8
JLEQ L12
CONST 12
LDLW -12
GLOBAL Out.Int
CALL 2
CONST 12
LDLW -8
GLOBAL Out.Int
CALL 2
GLOBAL Out.Ln
CALL 0
!       ASSERT(c*B <= t) END;
LDLW -12
CONST 10000
TIMES
LDLW -8
JLEQ L12
CONST 0
CONST 34
GLOBAL EASSERT
CALL 2
LABEL L12
!     a[i] := t - c*B
LDLW -8
LDLW -12
CONST 10000
TIMES
MINUS
LDLW 16
LDLW -4
CONST 4
BOUND 35
STIW
!   FOR i := 0 TO N-1 DO
INCL -4
JUMP L8
LABEL L9
!   ASSERT(c = 0)
LDLW -12
JEQZ L16
CONST 0
CONST 37
GLOBAL EASSERT
CALL 2
LABEL L16
RETURN
END

PROC tRand07.next 4 5 0
! PROCEDURE next(): INTEGER;
!   Combine(a, u, 0, m);
GLOBAL tRand07.m
CONST 0
GLOBAL tRand07.u
CONST 48271
GLOBAL tRand07.Combine
CALL 4
LABEL L17
!   WHILE ~ Greater(m, u) DO
GLOBAL tRand07.u
GLOBAL tRand07.m
GLOBAL tRand07.Greater
CALLW 2
JNEQZ L19
!     IF (u[3] > 0) OR (u[2] > m[2]) THEN
GLOBAL tRand07.u
LDNW 12
JGTZ L21
GLOBAL tRand07.u
LDNW 8
GLOBAL tRand07.m
LDNW 8
JLEQ L22
LABEL L21
! 	w := (B*u[3] + u[2]) DIV (m[2] + 1)
GLOBAL tRand07.u
LDNW 12
CONST 10000
TIMES
GLOBAL tRand07.u
LDNW 8
PLUS
GLOBAL tRand07.m
LDNW 8
INC
ZCHECK 67
DIV
STLW -4
JUMP L20
LABEL L22
!         w := 1
CONST 1
STLW -4
LABEL L20
!     IF w > B THEN w := B END;
LDLW -4
CONST 10000
JLEQ L26
CONST 10000
STLW -4
LABEL L26
!     Combine(1, u, -w, m);
GLOBAL tRand07.m
LDLW -4
UMINUS
GLOBAL tRand07.u
CONST 1
GLOBAL tRand07.Combine
CALL 4
JUMP L17
LABEL L19
!   RETURN u[2]*B*B + u[1]*B + u[0]
GLOBAL tRand07.u
LDNW 8
CONST 100000000
TIMES
GLOBAL tRand07.u
LDNW 4
CONST 10000
TIMES
PLUS
LDGW tRand07.u
PLUS
RETURN
END

PROC tRand07.%main 0 4 0
!   u[3] := 0; u[2] := 0; u[1] := 3141; u[0] := 5926;
CONST 0
GLOBAL tRand07.u
STNW 12
CONST 0
GLOBAL tRand07.u
STNW 8
CONST 3141
GLOBAL tRand07.u
STNW 4
CONST 5926
STGW tRand07.u
!   m[3] := 0; m[2] := 21; m[1] := 4748; m[0] := 3647; 
CONST 0
GLOBAL tRand07.m
STNW 12
CONST 21
GLOBAL tRand07.m
STNW 8
CONST 4748
GLOBAL tRand07.m
STNW 4
CONST 3647
STGW tRand07.m
!   FOR x := 1 TO 100 DO 
CONST 1
STGW tRand07.x
LABEL L27
LDGW tRand07.x
CONST 100
JGT L28
!     y := Random.Random();
GLOBAL Random.Random
CALLW 0
STGW tRand07.y
!     z := next();
GLOBAL tRand07.next
CALLW 0
STGW tRand07.z
!     Out.Int(y, 10); Out.Int(z, 12); Out.Ln;
CONST 10
LDGW tRand07.y
GLOBAL Out.Int
CALL 2
CONST 12
LDGW tRand07.z
GLOBAL Out.Int
CALL 2
GLOBAL Out.Ln
CALL 0
!     ASSERT(y = z)
LDGW tRand07.y
LDGW tRand07.z
JEQ L30
CONST 0
CONST 96
GLOBAL EASSERT
CALL 2
LABEL L30
!   FOR x := 1 TO 100 DO 
LDGW tRand07.x
INC
STGW tRand07.x
JUMP L27
LABEL L28
!   Out.Ln;
GLOBAL Out.Ln
CALL 0
!   FOR x := 1 TO 100000 DO ASSERT(Random.Random() = next()) END;
CONST 1
STGW tRand07.x
LABEL L31
LDGW tRand07.x
CONST 100000
JGT L32
GLOBAL Random.Random
CALLW 0
GLOBAL tRand07.next
CALLW 0
JEQ L34
CONST 0
CONST 101
GLOBAL EASSERT
CALL 2
LABEL L34
LDGW tRand07.x
INC
STGW tRand07.x
JUMP L31
LABEL L32
!   FOR x := 1 TO 20 DO
CONST 1
STGW tRand07.x
LABEL L35
LDGW tRand07.x
CONST 20
JGT L36
!     r := Random.Uniform();
GLOBAL Random.Uniform
CALLF 0
STGF tRand07.r
!     Out.Real(r, 0); Out.Ln;
CONST 0
LDGF tRand07.r
GLOBAL Out.Real
CALL 2
GLOBAL Out.Ln
CALL 0
!     ASSERT((0.0 <= r) & (r < 1.0))
LDGF tRand07.r
FCONST 0.0
FJNGEQ L37
LDGF tRand07.r
FCONST 1.0
FJLT L38
LABEL L37
CONST 0
CONST 107
GLOBAL EASSERT
CALL 2
LABEL L38
!   FOR x := 1 TO 20 DO
LDGW tRand07.x
INC
STGW tRand07.x
JUMP L35
LABEL L36
!   Out.Ln;
GLOBAL Out.Ln
CALL 0
!   FOR x := 1 TO 20 DO
CONST 1
STGW tRand07.x
LABEL L40
LDGW tRand07.x
CONST 20
JGT L41
!     y := Random.Roll(100000);
CONST 100000
GLOBAL Random.Roll
CALLW 1
STGW tRand07.y
!     Out.Int(y, 0); Out.Ln;
CONST 0
LDGW tRand07.y
GLOBAL Out.Int
CALL 2
GLOBAL Out.Ln
CALL 0
!     ASSERT((0 <= y) & (y < 100000))
LDGW tRand07.y
JLTZ L42
LDGW tRand07.y
CONST 100000
JLT L43
LABEL L42
CONST 0
CONST 115
GLOBAL EASSERT
CALL 2
LABEL L43
!   FOR x := 1 TO 20 DO
LDGW tRand07.x
INC
STGW tRand07.x
JUMP L40
LABEL L41
RETURN
END

! Global variables
GLOVAR tRand07.u 16
GLOVAR tRand07.m 16
GLOVAR tRand07.x 4
GLOVAR tRand07.y 4
GLOVAR tRand07.z 4
GLOVAR tRand07.r 4

! End of file
]]*)
