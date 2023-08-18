MODULE tHennessy;
(*  This is a suite of benchmarks that are relatively short, both in program
    size and execution time.  It requires no input, and prints the execution
    time for each program, using the system- dependent routine Getclock,
    below, to find out the current CPU time.  It does a rudimentary check to
    make sure each program gets the right output.  These programs were
    gathered by John Hennessy and modified by Peter Nye.
    Oberon: J.Templ 26.2.90 
    native Oberon: pm 09.12.95
    native Oberon OP2: pm 12.02.96
    adjusted for OBC: jms 09.10.18 *)

IMPORT
	Timer, Out;

CONST
	bubblebase = 1.61;
	dnfbase = 3.5;
	permbase = 1.75;
	queensbase = 1.83;
	towersbase = 2.39;
	quickbase = 1.92;
	intmmbase = 1.46;
	treebase =  2.5;
	mmbase = 0.0 (* 0.73 *);
	fpmmbase = 2.92;
	puzzlebase = 0.5;
	fftbase = 0.0 (* 1.11 *);
	fpfftbase = 4.44;

	(* Towers *)
	maxcells = 18;
	stackrange = (*0..*) 3;

	(* Intmm, Mm *)
	rowsize = 40;

	(* Puzzle *)
	size = 511;
	classmax = 3;
	typemax = 12;
	d = 8;

	(* Bubble, Quick *)
	sortelements = 5000;
	srtelements = 500;

	(* fft *)
	fftsize = 256;
	fftsize2 = 129;

	(* Perm *)
	permrange = (*0 ..*)10;
    (* Towers *)

TYPE
	(* tree *)
	node = POINTER TO nodeDesc;
	nodeDesc = RECORD
		left, right: node;
		val: INTEGER;
	END;
	
	(* Towers
	discsizrange = 1..maxcells;
	cellcursor = 0..maxcells; *)
	element = RECORD
		discsize: INTEGER;
		next: INTEGER;
	END ;
	
(*    emsgtype = packed array[1..15] of char;
*)
    (* Intmm, Mm *) (*
    index = 1 .. rowsize; *)
    intmatrix = ARRAY rowsize+1,rowsize+1 OF INTEGER;
    realmatrix = ARRAY rowsize+1,rowsize+1 OF REAL;

    (* Puzzle *) (*
    piececlass = 0..classmax;
    piecetype = 0..typemax;
    position = 0..size;
*)
    (* Bubble, Quick *) (*
    listsize = 0..sortelements;
    sortarray = array [listsize] of integer;
*)
    (* FFT *)
	complex = RECORD
		rp, ip: REAL
	END;
    carray = ARRAY fftsize+1 OF complex ;
    c2array = ARRAY fftsize2+1 OF complex ;

	Proc = PROCEDURE;
	
VAR
	fixed,floated: REAL;

    (* global *)
	seed: INTEGER;

    (* Perm *)
	permarray: ARRAY permrange+1 OF INTEGER;
	pctr: INTEGER;

    (* tree *)
	tree: node;

	(* Towers *)
    stack: ARRAY stackrange+1 OF INTEGER;
	cellspace: ARRAY maxcells+1 OF element;
	freelist: INTEGER;
	movesdone: INTEGER;

	(* Intmm, Mm *)
	ima, imb, imr: intmatrix;	(*Nun lokal definiert PM 24.09.94*)
	rma, rmb, rmr: realmatrix;

	(* Puzzle *)
	piececount: ARRAY classmax+1 OF INTEGER;
	class, piecemax: ARRAY typemax+1 OF INTEGER;
	puzzl: ARRAY size+1 OF BOOLEAN;
	p: ARRAY typemax+1, size+1 OF BOOLEAN;
	n,
	kount: INTEGER;

	(* Bubble, Quick *)
	sortlist: ARRAY sortelements+1 OF INTEGER;
    biggest, littlest,
    top: INTEGER;

    (* FFT *)
	z, w: carray;
    e: c2array;
	zr, zi: REAL;

(* global procedures *)

PROCEDURE Str*(s: ARRAY OF CHAR);
	VAR i: INTEGER;
BEGIN
	i:=0;
	WHILE s[i] # 0X DO
		IF s[i]="$" THEN Out.Ln ELSE Out.Char(s[i]) END;
		INC(i)
	END
END Str;

PROCEDURE Getclock (): INTEGER;
BEGIN
	RETURN Timer.Now()
END Getclock;

PROCEDURE Initrand ();
BEGIN seed := 74755
END Initrand;

PROCEDURE Rand (): INTEGER;
BEGIN
    seed := (seed * 1309 + 13849) MOD 65535;
    RETURN (seed);
END Rand;


    (* Permutation program, heavily recursive, written by Denny Brown. *)
	
	PROCEDURE Swap (VAR a,b: INTEGER);
		VAR t: INTEGER;
	BEGIN t := a;  a := b;  b := t;
	END Swap;
	
	PROCEDURE Initialize ();
		VAR i: INTEGER;
	BEGIN i := 1;
		WHILE i <= 7 DO
			permarray[i] := i-1;
			INC(i)
		END
	END Initialize;
	
	PROCEDURE Permute (n: INTEGER);
		VAR k: INTEGER;
	BEGIN
		pctr := pctr + 1;
		IF ( n#1 ) THEN
			Permute(n-1);
			k := n-1;
			WHILE k >= 1 DO
				Swap(permarray[n], permarray[k]);
				Permute(n-1);
				Swap(permarray[n], permarray[k]);
				DEC(k)
			END
       END
	END Permute;
	
PROCEDURE Perm ();
	VAR i: INTEGER;
BEGIN
	pctr := 0; i := 1;
	WHILE i <= 5 DO
		Initialize();
		Permute(7);
		INC(i)
	END ;
	IF ( pctr # 43300 ) THEN Str(" Error in Perm.$") END
END Perm;


    (*  Program to Solve the Towers of Hanoi *)

	PROCEDURE Makenull (s: INTEGER);
	BEGIN stack[s] := 0
	END Makenull;

	PROCEDURE Getelement (): INTEGER;
		VAR temp: INTEGER;
	BEGIN
		IF ( freelist>0 ) THEN
			temp := freelist;
			freelist := cellspace[freelist].next;
		ELSE
			Str("out of space   $")
		END ;
		RETURN (temp);
	END Getelement;
	
	PROCEDURE Push(i,s: INTEGER);
        VAR localel: INTEGER; errorfound: BOOLEAN;
	BEGIN
		errorfound := FALSE;
		IF ( stack[s] > 0 ) THEN
			IF ( cellspace[stack[s]].discsize<=i ) THEN
				errorfound := TRUE;
				Str("disc size error$")
			END 
		END ;
		IF ( ~ errorfound ) THEN
			localel := Getelement();
			cellspace[localel].next := stack[s];
			stack[s] := localel;
			cellspace[localel].discsize := i
		END
	END Push;
	
	PROCEDURE Init (s,n: INTEGER);
		VAR discctr: INTEGER;
	BEGIN
		Makenull(s); discctr := n;
		WHILE discctr >= 1 DO
			Push(discctr,s);
			DEC(discctr)
		END
	END Init;

	PROCEDURE Pop (s: INTEGER): INTEGER;
		VAR temp, temp1: INTEGER;
	BEGIN
		IF ( stack[s] > 0 ) THEN
			temp1 := cellspace[stack[s]].discsize;
			temp := cellspace[stack[s]].next;
			cellspace[stack[s]].next := freelist;
			freelist := stack[s];
			stack[s] := temp;
			RETURN (temp1)
		ELSE
			Str("nothing to pop $")
		END
	END Pop;

	PROCEDURE Move (s1,s2: INTEGER);
	BEGIN
		Push(Pop(s1),s2);
		movesdone := movesdone+1;
	END Move;

	PROCEDURE tower(i,j,k: INTEGER);
		VAR other: INTEGER;
	BEGIN
		IF ( k=1 ) THEN
			Move(i,j);
		ELSE
			other := 6-i-j;
			tower(i,other,k-1);
			Move(i,j);
			tower(other,j,k-1)
		END
	END tower;

PROCEDURE Towers ();
	VAR i: INTEGER;
BEGIN i := 1;
	WHILE i <= maxcells DO cellspace[i].next := i-1; INC(i) END ;
	freelist := maxcells;
	Init(1,14);
	Makenull(2);
	Makenull(3);
	movesdone := 0;
	tower(1,2,14);
	IF ( movesdone # 16383 ) THEN Str(" Error in Towers.$") END
END Towers;


    (* The eight queens problem, solved 50 times. *)
(*
  type
      doubleboard =   2..16;
      doublenorm  =   -7..7;
      boardrange  =   1..8;
      aarray      =   array [boardrange] of boolean;
      barray      =   array [doubleboard] of boolean;
      carray      =   array [doublenorm] of boolean;
      xarray      =   array [boardrange] of boardrange;
*)

	PROCEDURE Try(i: INTEGER; VAR q: BOOLEAN; VAR a, b, c: ARRAY OF BOOLEAN; VAR x: ARRAY OF INTEGER);
    	VAR j: INTEGER;
	BEGIN
		j := 0;
		q := FALSE;
		WHILE (~q) & (j # 8) DO
			j := j + 1;
			q := FALSE;
			IF b[j] & a[i+j] & c[i-j+7] THEN
				x[i] := j;
				b[j] := FALSE;
				a[i+j] := FALSE;
				c[i-j+7] := FALSE;
				IF i < 8 THEN
					Try(i+1,q,a,b,c,x);
					IF ~q THEN
						b[j] := TRUE;
						a[i+j] := TRUE;
						c[i-j+7] := TRUE
					END
     	   	ELSE q := TRUE
				END
			END
		END
	END Try;

	PROCEDURE Doit ();
		VAR i: INTEGER; q: BOOLEAN;
			a: ARRAY 9 OF BOOLEAN;
			b: ARRAY 17 OF BOOLEAN;
			c: ARRAY 15 OF BOOLEAN;
			x: ARRAY 9 OF INTEGER;
	BEGIN
		i := 0 - 7;
		WHILE i <= 16 DO
			IF (i >= 1) & (i <= 8) THEN a[i] := TRUE END ;
			IF i >= 2 THEN b[i] := TRUE END ;
			IF i <= 7 THEN c[i+7] := TRUE END ;
			i := i + 1;
		END ;
		Try(1, q, b, a, c, x);
		IF ( ~ q ) THEN Str(" Error in Queens.$") END
	END Doit;

PROCEDURE Queens ();
	VAR i: INTEGER;
BEGIN i := 1;
	WHILE i <= 50 DO Doit(); INC(i) END
END Queens;


    (* Multiplies two integer matrices. *)

	PROCEDURE Initmatrix (VAR m: intmatrix);
		VAR temp, i, j: INTEGER;
	BEGIN i := 1;
		WHILE i <= rowsize DO
			j := 1;
			WHILE j <= rowsize DO
				temp := Rand();
				m[i][j] := temp - (temp DIV 120)*120 - 60;
				INC(j)
			END ;
			INC(i)
		END
	END Initmatrix;

	PROCEDURE Innerproduct(VAR result: INTEGER; VAR a,b: intmatrix; row,column: INTEGER);
		VAR i: INTEGER;
  (* computes the inner product of A[row,*] and B[*,column] *)
	BEGIN
		result := 0; i := 1;
		WHILE i <= rowsize DO result := result+a[row][i]*b[i][column]; INC(i) END
	END Innerproduct;

PROCEDURE Intmm ();
	VAR i, j: INTEGER; (*ima, imb, imr: intmatrix;*)
BEGIN
	Initrand();
	Initmatrix (ima);
	Initmatrix (imb);
	i := 1;
	WHILE i <= rowsize DO j := 1;
		WHILE j <= rowsize DO Innerproduct(imr[i][j],ima,imb,i,j); INC(j) END ;
		INC(i)
	END
END Intmm;


    (* Multiplies two real matrices. *)

	PROCEDURE rInitmatrix (VAR m: realmatrix);
		VAR temp, i, j: INTEGER;
	BEGIN i := 1;
		WHILE i <= rowsize DO j := 1;
			WHILE j <= rowsize DO
				temp := Rand();
				m[i][j] := (temp - (temp DIV 120)*120 - 60) DIV 3;
				INC(j)
			END ;
			INC(i)
		END
	END rInitmatrix;

	PROCEDURE rInnerproduct(VAR result: REAL; VAR a,b: realmatrix; row,column: INTEGER);
	(* computes the inner product of A[row,*] and B[*,column] *)
		VAR i: INTEGER;
	BEGIN
		result := 0.0; i := 1;
		WHILE i<=rowsize DO result := result+a[row][i]*b[i][column]; INC(i) END
	END rInnerproduct;

PROCEDURE Mm ();
	VAR i, j: INTEGER; (*rma, rmb, rmr: realmatrix;*)
BEGIN
	Initrand();
	rInitmatrix (rma);
	rInitmatrix (rmb);
	i := 1;
	WHILE i <= rowsize DO j := 1;
		WHILE j <= rowsize DO rInnerproduct(rmr[i][j],rma,rmb,i,j); INC(j) END ;
		INC(i)
	END
END Mm;

    (* A compute-bound program from Forest Baskett. *)

	PROCEDURE Fit (i, j: INTEGER): BOOLEAN;
		VAR k: INTEGER;
	BEGIN k := 0;
		WHILE k <= piecemax[i] DO
			IF ( p[i][k] ) THEN IF ( puzzl[j+k] ) THEN RETURN FALSE END END;
			INC(k)
		END;
		RETURN TRUE
	END Fit;
	
	PROCEDURE Place (i, j: INTEGER): INTEGER;
		VAR k: INTEGER;
	BEGIN k := 0;
		WHILE k <= piecemax[i] DO
			IF ( p[i][k] ) THEN puzzl[j+k] := TRUE END;
			INC(k)
		END;
		piececount[class[i]] := piececount[class[i]] - 1;
		k := j;
		WHILE k <= size DO
			IF ( ~ puzzl[k] ) THEN RETURN (k) END;
			INC(k)
		END ;
		RETURN (0);
	END Place;
	
	PROCEDURE Remove (i, j: INTEGER);
		VAR k: INTEGER;
	BEGIN k := 0;
		WHILE k <= piecemax[i] DO
			IF ( p[i][k] ) THEN puzzl[j+k] := FALSE END;
			INC(k)
		END;
		piececount[class[i]] := piececount[class[i]] + 1
	END Remove;
	
	PROCEDURE Trial (j: INTEGER): BOOLEAN;
		VAR i, k: INTEGER;
	BEGIN i := 0;
		kount := kount + 1;
		WHILE i <= typemax DO
			IF ( piececount[class[i]] # 0 ) THEN
				IF ( Fit (i, j) ) THEN
					k := Place (i, j);
					IF Trial(k) OR (k = 0) THEN RETURN (TRUE)
					ELSE Remove (i, j)
					END;
				END
			END;
			INC(i)
		END;
		RETURN (FALSE)
	END Trial;
	
PROCEDURE Puzzle ();
	VAR i, j, k, m: INTEGER;
BEGIN
	m := 0; WHILE m <= size DO puzzl[m] := TRUE; INC(m) END ;
	i := 1;
	WHILE i <= 5 DO j := 1;
		WHILE j <= 5 DO k := 1;
			WHILE k <= 5 DO
				puzzl[i+d*(j+d*k)] := FALSE; INC(k)
			END;
			INC(j)
		END;
		INC(i)
	END;
	
	i := 0; 
	WHILE i <= typemax DO m := 0;
		WHILE m<= size DO
			p[i][m] := FALSE; INC(m)
		END;
		INC(i)
	END;
	
	i := 0;
	WHILE i <= 3 DO j := 0;
		WHILE j <= 1 DO k := 0;
			WHILE k <= 0 DO
				p[0][i+d*(j+d*k)] := TRUE; INC(k)
			END;
			INC(j)
		END;
		INC(i)
	END;
	class[0] := 0;
	piecemax[0] := 3+d*1+d*d*0;
	
	i := 0;
	WHILE i <= 1 DO j := 0;
		WHILE j <= 0 DO k := 0;
			WHILE k <= 3 DO
				p[1][i+d*(j+d*k)] := TRUE; INC(k)
			END;
			INC(j)
		END;
		INC(i)
	END;
	class[1] := 0;
	piecemax[1] := 1+d*0+d*d*3;
	
	i := 0;
	WHILE i <= 0 DO j := 0;
		WHILE j <= 3 DO k := 0;
			WHILE k <= 1 DO
				p[2][i+d*(j+d*k)] := TRUE; INC(k)
			END;
			INC(j)
		END;
		INC(i)
	END;
	class[2] := 0;
	piecemax[2] := 0+d*3+d*d*1;
	
	i := 0;
	WHILE i <= 1 DO j := 0;
		WHILE j <= 3 DO k := 0;
			WHILE k <= 0 DO
				p[3][i+d*(j+d*k)] := TRUE; INC(k)
			END;
			INC(j)
		END;
		INC(i)
	END;
	class[3] := 0;
	piecemax[3] := 1+d*3+d*d*0;

	i := 0;
	WHILE i <= 3 DO j := 0;
		WHILE j <= 0 DO k := 0;
			WHILE k <= 1 DO
				p[4][i+d*(j+d*k)] := TRUE; INC(k)
			END;
			INC(j)
		END;
		INC(i)
	END;
	class[4] := 0;
	piecemax[4] := 3+d*0+d*d*1;
	
	i := 0;
	WHILE i <= 0 DO j := 0;
		WHILE j <= 1 DO k := 0;
			WHILE k <= 3 DO
				p[5][i+d*(j+d*k)] := TRUE; INC(k)
			END;
			INC(j)
		END;
		INC(i)
	END;
	class[5] := 0;
	piecemax[5] := 0+d*1+d*d*3;
	
	i := 0;
	WHILE i <= 2 DO j := 0;
		WHILE j <= 0 DO k := 0;
			WHILE k <= 0 DO
				p[6][i+d*(j+d*k)] := TRUE; INC(k)
			END;
			INC(j)
		END;
		INC(i)
	END;
	class[6] := 1;
	piecemax[6] := 2+d*0+d*d*0;
	
	i := 0;
	WHILE i <= 0 DO j := 0;
		WHILE j <= 2 DO k := 0;
			WHILE k <= 0 DO
				p[7][i+d*(j+d*k)] := TRUE; INC(k)
			END;
			INC(j)
		END;
		INC(i)
	END;
	class[7] := 1;
	piecemax[7] := 0+d*2+d*d*0;
	
	i := 0;
	WHILE i <= 0 DO j := 0;
		WHILE j <= 0 DO k := 0;
			WHILE k <= 2 DO
				p[8][i+d*(j+d*k)] := TRUE; INC(k)
			END;
			INC(j)
		END;
		INC(i)
	END;
	class[8] := 1;
    piecemax[8] := 0+d*0+d*d*2;
	
	i := 0;
	WHILE i <= 1 DO j := 0;
		WHILE j <= 1 DO k := 0;
			WHILE k <= 0 DO
				p[9][i+d*(j+d*k)] := TRUE; INC(k)
			END;
			INC(j)
		END;
		INC(i)
	END;
	class[9] := 2;
	piecemax[9] := 1+d*1+d*d*0;
	
	i := 0;
	WHILE i <= 1 DO j := 0;
		WHILE j <= 0 DO k := 0;
			WHILE k <= 1 DO
				p[10][i+d*(j+d*k)] := TRUE; INC(k)
			END;
			INC(j)
		END;
		INC(i)
	END;
	class[10] := 2;
	piecemax[10] := 1+d*0+d*d*1;
	
	i := 0;
	WHILE i <= 0 DO j := 0;
		WHILE j <= 1 DO k := 0;
			WHILE k <= 1 DO
				p[11][i+d*(j+d*k)] := TRUE; INC(k)
			END;
			INC(j)
		END;
		INC(i)
	END;
	class[11] := 2;
	piecemax[11] := 0+d*1+d*d*1;
	
	i := 0;
	WHILE i <= 1 DO j := 0;
		WHILE j <= 1 DO k := 0;
			WHILE k <= 1 DO
				p[12][i+d*(j+d*k)] := TRUE; INC(k)
			END;
			INC(j)
		END;
		INC(i)
	END;
	class[12] := 3;
	piecemax[12] := 1+d*1+d*d*1;
	
	piececount[0] := 13;
	piececount[1] := 3;
	piececount[2] := 1;
	piececount[3] := 1;
	m := 1+d*(1+d*1);
	kount := 0;
	IF Fit(0, m) THEN n := Place(0, m)
	ELSE Str("Error1 in Puzzle$")
	END;
	IF ~ Trial(n) THEN Str("Error2 in Puzzle.$")
	ELSIF kount # 2005 THEN Str("Error3 in Puzzle.$")
	END
END Puzzle;


   (* Sorts an array using quicksort *)

	PROCEDURE Initarr();
		VAR i, temp: INTEGER;
	BEGIN
		Initrand();
		biggest := 0; littlest := 0; i := 1;
		WHILE i <= sortelements DO
			temp := Rand();
			sortlist[i] := temp - (temp DIV 100000)*100000 - 50000;
			IF sortlist[i] > biggest THEN biggest := sortlist[i]
			ELSIF sortlist[i] < littlest THEN littlest := sortlist[i]
			END ;
			INC(i)
		END
	END Initarr;

	PROCEDURE Quicksort(VAR a: ARRAY OF INTEGER; l,r: INTEGER);
  (* quicksort the array A from start to finish *)
		VAR i,j,x,w: INTEGER;
	BEGIN
		i:=l; j:=r;
		x:=a[(l+r) DIV 2];
		REPEAT
			WHILE a[i]<x DO i := i+1 END;
			WHILE x<a[j] DO j := j-1 END;
			IF i<=j THEN
				w := a[i];
				a[i] := a[j];
				a[j] := w;
				i := i+1;    j := j-1
			END ;
		UNTIL i > j;
		IF l<j THEN Quicksort(a,l,j) END;
		IF i<r THEN Quicksort(a,i,r) END
	END Quicksort;

PROCEDURE Quick ();
BEGIN
    Initarr();
    Quicksort(sortlist,1,sortelements);
    IF (sortlist[1] # littlest) OR (sortlist[sortelements] # biggest) THEN  Str( " Error in Quick.$") END ;
END Quick;


    (* Sorts an array using bubblesort *)

	PROCEDURE bInitarr();
		VAR i, temp: INTEGER;
	BEGIN
		Initrand();
		biggest := 0; littlest := 0; i := 1;
		WHILE i <= srtelements DO
			temp := Rand();
			sortlist[i] := temp - (temp DIV 100000)*100000 - 50000;
			IF sortlist[i] > biggest THEN biggest := sortlist[i]
			ELSIF sortlist[i] < littlest THEN littlest := sortlist[i]
			END ;
			INC(i)
		END
	END bInitarr;

PROCEDURE Bubble();
	VAR i, j: INTEGER;
BEGIN
	bInitarr();
	top:=srtelements;
	WHILE top>1 DO
		i:=1;
		WHILE i<top DO
			IF sortlist[i] > sortlist[i+1] THEN
				j := sortlist[i];
				sortlist[i] := sortlist[i+1];
				sortlist[i+1] := j;
			END ;
			i:=i+1;
		END;
		top:=top-1;
	END;
	IF (sortlist[1] # littlest) OR (sortlist[srtelements] # biggest) THEN Str("Error3 in Bubble.$") END ;
END Bubble;

    (* Sorts an array using treesort *)

	PROCEDURE tInitarr();
		VAR i, temp: INTEGER;
	BEGIN
		Initrand();
		biggest := 0; littlest := 0; i := 1;
		WHILE i <= sortelements DO
			temp := Rand();
			sortlist[i] := temp - (temp DIV 100000)*100000 - 50000;
			IF sortlist[i] > biggest THEN biggest := sortlist[i]
			ELSIF sortlist[i] < littlest THEN littlest := sortlist[i]
			END ;
			INC(i)
		END
	END tInitarr;

	PROCEDURE CreateNode (VAR t: node; n: INTEGER);
	BEGIN
    	NEW(t);
		t.left := NIL; t.right := NIL;
		t.val := n
	END CreateNode;

	PROCEDURE Insert(n: INTEGER; t: node);
	(* insert n into tree *)
	BEGIN
		IF n > t.val THEN
			IF t.left = NIL THEN CreateNode(t.left,n)
			ELSE Insert(n,t.left)
			END
		ELSIF n < t.val THEN
			IF t.right = NIL THEN CreateNode(t.right,n)
			ELSE Insert(n,t.right)
			END
		END
	END Insert;

	PROCEDURE Checktree(p: node): BOOLEAN;
    (* check by inorder traversal *)
		VAR result: BOOLEAN;
	BEGIN
		result := TRUE;
		IF p.left # NIL THEN
			IF p.left.val <= p.val THEN result := FALSE;
			ELSE result := Checktree(p.left) & result
			END
		END ;
		IF  p.right # NIL THEN
			IF p.right.val >= p.val THEN result := FALSE;
			ELSE result := Checktree(p.right) & result
			END
		END;
		RETURN result
	END Checktree;

PROCEDURE Trees();
	VAR i: INTEGER;
BEGIN
	tInitarr();
	NEW(tree);
	tree.left := NIL; tree.right:=NIL; tree.val:=sortlist[1];
	i := 2;
    WHILE i <= sortelements DO
    	Insert(sortlist[i],tree);
    	INC(i)
    END;
	IF ~ Checktree(tree) THEN Str(" Error in Tree.$") END;
END Trees;

	PROCEDURE Cos (x: REAL): REAL;
	(* computes cos of x (x in radians) by an expansion *)
		VAR i, factor: INTEGER;
			result,power: REAL;
	BEGIN
		result := 1.0; factor := 1;  power := x; i := 2;
		WHILE i <= 10 DO
			factor := factor * i;  power := power*x;
			IF i MOD 2 = 0 THEN
				IF i MOD 4 = 0 THEN result := result + power/factor
				ELSE result := result - power/factor
				END
			END;
			INC(i)
		END ;
		RETURN result
	END Cos;
	
	PROCEDURE Min0( arg1, arg2: INTEGER): INTEGER;
	BEGIN
		IF arg1 < arg2 THEN RETURN arg1
		ELSE RETURN arg2
		END
	END Min0;

	PROCEDURE Uniform11(iy: INTEGER; yfl: REAL);
	BEGIN
		iy := (4855*iy + 1731) MOD 8192;
		yfl := iy/8192.0;
	END Uniform11;

	PROCEDURE Exptab(n: INTEGER; VAR e: c2array);
		VAR theta, divisor, d3: REAL; h: ARRAY 26 OF REAL; (* d1 by pm 12.02.96 *)
			i, j, k, l, m, d1, d2: INTEGER;
	BEGIN
		theta := 3.1415926536;
		divisor := 4.0; i:=1;
		WHILE i <= 25 DO
			h[i] := 1/(2*Cos( theta/divisor ));
			divisor := divisor + divisor;
			INC(i)
		END;
		m := n DIV 2 ;
		l := m DIV 2 ;
		j := 1 ;
		e[1].rp := 1.0 ;
		e[1].ip := 0.0;
		e[l+1].rp := 0.0;
		e[l+1].ip := 1.0 ;
		e[m+1].rp := -1.0 ;
		e[m+1].ip := 0.0 ;
		REPEAT
			i := l DIV 2 ;
			k := i ;
			REPEAT
				d1:= k+i+1; d2:= k-i+1; d3:= e[ d2].rp;
				e[k+1].rp := h[j]*(e[ d1].rp+d3) ; d3:= e[ d2].ip;
				e[k+1].ip := h[j]*(e[ d1].ip+d3) ;
				k := k+l ;
			UNTIL ( k > m );
			j := Min0( j+1, 25);
			l := i ;
		UNTIL ( l <= 1 );
	END Exptab;

	PROCEDURE Fft( n: INTEGER; VAR z, w: carray; VAR e: c2array; sqrinv: REAL);
		VAR i, j, k, l, m, index: INTEGER; h, d1: REAL;	(* d1, d2 added pm 12.02.96*)
	BEGIN
		m := n DIV 2 ;
		l := 1 ;
		REPEAT
			k := 0 ;
			j := l ;
			i := 1 ;
			REPEAT
				REPEAT
					w[i+k].rp := z[i].rp+z[m+i].rp ;
					w[i+k].ip := z[i].ip+z[m+i].ip ;
					h := e[k+1].rp*(z[i].rp-z[i+m].rp);	d1:= (z[i].ip-z[i+m].ip);
					w[i+j].rp := h-e[k+1].ip*d1 ;
					h := e[k+1].rp*(z[i].ip-z[i+m].ip);	d1:=(z[i].rp-z[i+m].rp);
					w[i+j].ip := h+e[k+1].ip*d1 ;
					i := i+1 ;
				UNTIL ( i > j );
				k := j ;
				j := k+l ;
			UNTIL ( j > m );
			(*z := w ;*) index := 1;
			REPEAT
				z[index] := w[index];
				index := index+1;
			UNTIL ( index > n );
			l := l+l ;
		UNTIL ( l > m );
		i := 1;
		WHILE i <= n DO
			z[i].rp := sqrinv*z[i].rp ;
			z[i].ip := -sqrinv*z[i].ip;
			INC(i)
		END
	END Fft;
	
PROCEDURE Oscar ();
	VAR i: INTEGER;
BEGIN
	Exptab(fftsize,e) ;
	seed := 5767 ; i := 1;
	WHILE i <= fftsize DO
		Uniform11( seed, zr );
		Uniform11( seed, zi );
		z[i].rp := 20.0*zr - 10.0;
		z[i].ip := 20.0*zi - 10.0;
		INC(i)
	END ;
	i := 1;
	WHILE i <= 20 DO Fft(fftsize,z,w,e,0.0625); INC(i) END
END Oscar;

PROCEDURE Time(s: ARRAY OF CHAR; p: Proc; base, fbase: REAL);
	VAR timer: INTEGER; i: INTEGER;
BEGIN
(*
	Str(s);
*)
	i := 0;
	timer := Getclock();
	WHILE i < 10 DO p; INC(i) END ;
	timer := (Getclock()-timer);
(*
	Out.Int(timer, 8); Out.Ln;
*)
	fixed := fixed + timer*base;
	floated := floated + timer*fbase
END Time;

PROCEDURE Do*;
BEGIN
	fixed := 0.0;  floated := 0.0;
	Time("Perm ", Perm, permbase, permbase);
	Time("Towers ", Towers, towersbase, towersbase);
	Time("Queens ", Queens, queensbase, queensbase);
	Time("Intmm ", Intmm, intmmbase, intmmbase);
	Time("Mm ", Mm, mmbase, fpmmbase);
	Time("Puzzle ", Puzzle, puzzlebase, puzzlebase);
	Time("Quick ", Quick, quickbase, quickbase);
	Time("Bubble ", Bubble, bubblebase, bubblebase);
	Time("Tree ", Trees, treebase, treebase);
	Time("FFT ", Oscar, fftbase, fpfftbase);
(*
	Str("Nonfloating point composite is "); Out.Real(fixed, 0); Out.Ln;
	Str("Floating point composite is "); Out.Real(floated, 0); Out.Ln
*)
 END Do;

BEGIN
	Do;
        Str("Done$")
END tHennessy.

(*<<
Done
>>*)

(*[[
!! (SYMFILE #tHennessy STAMP #tHennessy.%main 1 #tHennessy.m)
!! (DEF !1 (FLEX CHAR))
!! (PROCEDURE #Str* 140 #tHennessy.Str !2 (PROC 2 VOID
!!     (PARAM #s 12 =1)))
!! (PROCEDURE #Do* 992 #tHennessy.Do !3 (PROC 0 VOID))
!! (CHKSUM STAMP)
!! 
MODULE tHennessy STAMP 0
IMPORT Timer STAMP
IMPORT Out STAMP
ENDHDR

PROC tHennessy.Str 4 3 0
! PROCEDURE Str*(s: ARRAY OF CHAR);
LOCAL 12
LDLW 16
FLEXCOPY
! 	i:=0;
CONST 0
STLW -4
LABEL L24
! 	WHILE s[i] # 0X DO
LDLW 12
LDLW -4
LDLW 16
BOUND 144
LDIC
JEQZ L26
! 		IF s[i]="$" THEN Out.Ln ELSE Out.Char(s[i]) END;
LDLW 12
LDLW -4
LDLW 16
BOUND 145
LDIC
CONST 36
JNEQ L29
GLOBAL Out.Ln
CALL 0
JUMP L27
LABEL L29
LDLW 12
LDLW -4
LDLW 16
BOUND 145
LDIC
ALIGNC
GLOBAL Out.Char
CALL 1
LABEL L27
! 		INC(i)
INCL -4
JUMP L24
LABEL L26
RETURN
END

PROC tHennessy.Getclock 0 1 0
! PROCEDURE Getclock (): INTEGER;
! 	RETURN Timer.Now()
GLOBAL Timer.Now
CALLW 0
RETURN
END

PROC tHennessy.Initrand 0 2 0
! PROCEDURE Initrand ();
! BEGIN seed := 74755
CONST 74755
STGW tHennessy.seed
RETURN
END

PROC tHennessy.Rand 0 2 0
! PROCEDURE Rand (): INTEGER;
!     seed := (seed * 1309 + 13849) MOD 65535;
LDGW tHennessy.seed
CONST 1309
TIMES
CONST 13849
PLUS
CONST 65535
MOD
STGW tHennessy.seed
!     RETURN (seed);
LDGW tHennessy.seed
RETURN
END

PROC tHennessy.Swap 4 2 0x00300001
! 	PROCEDURE Swap (VAR a,b: INTEGER);
! 	BEGIN t := a;  a := b;  b := t;
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

PROC tHennessy.Initialize 4 4 0
! 	PROCEDURE Initialize ();
! 	BEGIN i := 1;
CONST 1
STLW -4
LABEL L30
! 		WHILE i <= 7 DO
LDLW -4
CONST 7
JGT L32
! 			permarray[i] := i-1;
LDLW -4
DEC
GLOBAL tHennessy.permarray
LDLW -4
CONST 11
BOUND 177
STIW
! 			INC(i)
INCL -4
JUMP L30
LABEL L32
RETURN
END

PROC tHennessy.Permute 4 4 0
! 	PROCEDURE Permute (n: INTEGER);
! 		pctr := pctr + 1;
LDGW tHennessy.pctr
INC
STGW tHennessy.pctr
! 		IF ( n#1 ) THEN
LDLW 12
CONST 1
JEQ L35
! 			Permute(n-1);
LDLW 12
DEC
GLOBAL tHennessy.Permute
CALL 1
! 			k := n-1;
LDLW 12
DEC
STLW -4
LABEL L36
! 			WHILE k >= 1 DO
LDLW -4
CONST 1
JLT L35
! 				Swap(permarray[n], permarray[k]);
GLOBAL tHennessy.permarray
LDLW -4
CONST 11
BOUND 190
INDEXW
GLOBAL tHennessy.permarray
LDLW 12
CONST 11
BOUND 190
INDEXW
GLOBAL tHennessy.Swap
CALL 2
! 				Permute(n-1);
LDLW 12
DEC
GLOBAL tHennessy.Permute
CALL 1
! 				Swap(permarray[n], permarray[k]);
GLOBAL tHennessy.permarray
LDLW -4
CONST 11
BOUND 192
INDEXW
GLOBAL tHennessy.permarray
LDLW 12
CONST 11
BOUND 192
INDEXW
GLOBAL tHennessy.Swap
CALL 2
! 				DEC(k)
DECL -4
JUMP L36
LABEL L35
RETURN
END

PROC tHennessy.Perm 4 3 0
! PROCEDURE Perm ();
! 	pctr := 0; i := 1;
CONST 0
STGW tHennessy.pctr
CONST 1
STLW -4
LABEL L39
! 	WHILE i <= 5 DO
LDLW -4
CONST 5
JGT L41
! 		Initialize();
GLOBAL tHennessy.Initialize
CALL 0
! 		Permute(7);
CONST 7
GLOBAL tHennessy.Permute
CALL 1
! 		INC(i)
INCL -4
JUMP L39
LABEL L41
! 	IF ( pctr # 43300 ) THEN Str(" Error in Perm.$") END
LDGW tHennessy.pctr
CONST 43300
JEQ L44
CONST 17
GLOBAL tHennessy.%1
GLOBAL tHennessy.Str
CALL 2
LABEL L44
RETURN
END

PROC tHennessy.Makenull 0 4 0
! 	PROCEDURE Makenull (s: INTEGER);
! 	BEGIN stack[s] := 0
CONST 0
GLOBAL tHennessy.stack
LDLW 12
CONST 4
BOUND 214
STIW
RETURN
END

PROC tHennessy.Getelement 4 3 0
! 	PROCEDURE Getelement (): INTEGER;
! 		IF ( freelist>0 ) THEN
LDGW tHennessy.freelist
JLEQZ L47
! 			temp := freelist;
LDGW tHennessy.freelist
STLW -4
! 			freelist := cellspace[freelist].next;
GLOBAL tHennessy.cellspace
LDGW tHennessy.freelist
CONST 19
BOUND 222
INDEXD
LDNW 4
STGW tHennessy.freelist
JUMP L45
LABEL L47
! 			Str("out of space   $")
CONST 17
GLOBAL tHennessy.%2
GLOBAL tHennessy.Str
CALL 2
LABEL L45
! 		RETURN (temp);
LDLW -4
RETURN
END

PROC tHennessy.Push 8 4 0
! 	PROCEDURE Push(i,s: INTEGER);
! 		errorfound := FALSE;
CONST 0
STLC -5
! 		IF ( stack[s] > 0 ) THEN
GLOBAL tHennessy.stack
LDLW 16
CONST 4
BOUND 233
LDIW
JLEQZ L50
! 			IF ( cellspace[stack[s]].discsize<=i ) THEN
GLOBAL tHennessy.cellspace
GLOBAL tHennessy.stack
LDLW 16
CONST 4
BOUND 234
LDIW
CONST 19
BOUND 234
INDEXD
LOADW
LDLW 12
JGT L50
! 				errorfound := TRUE;
CONST 1
STLC -5
! 				Str("disc size error$")
CONST 17
GLOBAL tHennessy.%3
GLOBAL tHennessy.Str
CALL 2
LABEL L50
! 		IF ( ~ errorfound ) THEN
LDLC -5
JNEQZ L56
! 			localel := Getelement();
GLOBAL tHennessy.Getelement
CALLW 0
STLW -4
! 			cellspace[localel].next := stack[s];
GLOBAL tHennessy.stack
LDLW 16
CONST 4
BOUND 241
LDIW
GLOBAL tHennessy.cellspace
LDLW -4
CONST 19
BOUND 241
INDEXD
STNW 4
! 			stack[s] := localel;
LDLW -4
GLOBAL tHennessy.stack
LDLW 16
CONST 4
BOUND 242
STIW
! 			cellspace[localel].discsize := i
LDLW 12
GLOBAL tHennessy.cellspace
LDLW -4
CONST 19
BOUND 243
INDEXD
STOREW
LABEL L56
RETURN
END

PROC tHennessy.Init 4 3 0
! 	PROCEDURE Init (s,n: INTEGER);
! 		Makenull(s); discctr := n;
LDLW 12
GLOBAL tHennessy.Makenull
CALL 1
LDLW 16
STLW -4
LABEL L57
! 		WHILE discctr >= 1 DO
LDLW -4
CONST 1
JLT L59
! 			Push(discctr,s);
LDLW 12
LDLW -4
GLOBAL tHennessy.Push
CALL 2
! 			DEC(discctr)
DECL -4
JUMP L57
LABEL L59
RETURN
END

PROC tHennessy.Pop 8 5 0
! 	PROCEDURE Pop (s: INTEGER): INTEGER;
! 		IF ( stack[s] > 0 ) THEN
GLOBAL tHennessy.stack
LDLW 12
CONST 4
BOUND 260
LDIW
JLEQZ L62
! 			temp1 := cellspace[stack[s]].discsize;
GLOBAL tHennessy.cellspace
GLOBAL tHennessy.stack
LDLW 12
CONST 4
BOUND 261
LDIW
CONST 19
BOUND 261
INDEXD
LOADW
STLW -8
! 			temp := cellspace[stack[s]].next;
GLOBAL tHennessy.cellspace
GLOBAL tHennessy.stack
LDLW 12
CONST 4
BOUND 262
LDIW
CONST 19
BOUND 262
INDEXD
LDNW 4
STLW -4
! 			cellspace[stack[s]].next := freelist;
LDGW tHennessy.freelist
GLOBAL tHennessy.cellspace
GLOBAL tHennessy.stack
LDLW 12
CONST 4
BOUND 263
LDIW
CONST 19
BOUND 263
INDEXD
STNW 4
! 			freelist := stack[s];
GLOBAL tHennessy.stack
LDLW 12
CONST 4
BOUND 264
LDIW
STGW tHennessy.freelist
! 			stack[s] := temp;
LDLW -4
GLOBAL tHennessy.stack
LDLW 12
CONST 4
BOUND 265
STIW
! 			RETURN (temp1)
LDLW -8
RETURN
LABEL L62
! 			Str("nothing to pop $")
CONST 17
GLOBAL tHennessy.%4
GLOBAL tHennessy.Str
CALL 2
ERROR E_RETURN 257
END

PROC tHennessy.Move 0 3 0
! 	PROCEDURE Move (s1,s2: INTEGER);
! 		Push(Pop(s1),s2);
LDLW 16
LDLW 12
GLOBAL tHennessy.Pop
CALLW 1
GLOBAL tHennessy.Push
CALL 2
! 		movesdone := movesdone+1;
LDGW tHennessy.movesdone
INC
STGW tHennessy.movesdone
RETURN
END

PROC tHennessy.tower 4 4 0
! 	PROCEDURE tower(i,j,k: INTEGER);
! 		IF ( k=1 ) THEN
LDLW 20
CONST 1
JNEQ L65
! 			Move(i,j);
LDLW 16
LDLW 12
GLOBAL tHennessy.Move
CALL 2
RETURN
LABEL L65
! 			other := 6-i-j;
CONST 6
LDLW 12
MINUS
LDLW 16
MINUS
STLW -4
! 			tower(i,other,k-1);
LDLW 20
DEC
LDLW -4
LDLW 12
GLOBAL tHennessy.tower
CALL 3
! 			Move(i,j);
LDLW 16
LDLW 12
GLOBAL tHennessy.Move
CALL 2
! 			tower(other,j,k-1)
LDLW 20
DEC
LDLW 16
LDLW -4
GLOBAL tHennessy.tower
CALL 3
RETURN
END

PROC tHennessy.Towers 4 4 0
! PROCEDURE Towers ();
! BEGIN i := 1;
CONST 1
STLW -4
LABEL L66
! 	WHILE i <= maxcells DO cellspace[i].next := i-1; INC(i) END ;
LDLW -4
CONST 18
JGT L68
LDLW -4
DEC
GLOBAL tHennessy.cellspace
LDLW -4
CONST 19
BOUND 294
INDEXD
STNW 4
INCL -4
JUMP L66
LABEL L68
! 	freelist := maxcells;
CONST 18
STGW tHennessy.freelist
! 	Init(1,14);
CONST 14
CONST 1
GLOBAL tHennessy.Init
CALL 2
! 	Makenull(2);
CONST 2
GLOBAL tHennessy.Makenull
CALL 1
! 	Makenull(3);
CONST 3
GLOBAL tHennessy.Makenull
CALL 1
! 	movesdone := 0;
CONST 0
STGW tHennessy.movesdone
! 	tower(1,2,14);
CONST 14
CONST 2
CONST 1
GLOBAL tHennessy.tower
CALL 3
! 	IF ( movesdone # 16383 ) THEN Str(" Error in Towers.$") END
LDGW tHennessy.movesdone
CONST 16383
JEQ L71
CONST 19
GLOBAL tHennessy.%5
GLOBAL tHennessy.Str
CALL 2
LABEL L71
RETURN
END

PROC tHennessy.Try 4 11 0x15600001
! 	PROCEDURE Try(i: INTEGER; VAR q: BOOLEAN; VAR a, b, c: ARRAY OF BOOLEAN; VAR x: ARRAY OF INTEGER);
! 		j := 0;
CONST 0
STLW -4
! 		q := FALSE;
CONST 0
LDLW 16
STOREC
LABEL L72
! 		WHILE (~q) & (j # 8) DO
LDLW 16
LOADC
JNEQZ L74
LDLW -4
CONST 8
JEQ L74
! 			j := j + 1;
INCL -4
! 			q := FALSE;
CONST 0
LDLW 16
STOREC
! 			IF b[j] & a[i+j] & c[i-j+7] THEN
LDLW 28
LDLW -4
LDLW 32
BOUND 325
LDIC
JEQZ L72
LDLW 20
LDLW 12
LDLW -4
PLUS
LDLW 24
BOUND 325
LDIC
JEQZ L72
LDLW 36
LDLW 12
LDLW -4
MINUS
CONST 7
PLUS
LDLW 40
BOUND 325
LDIC
JEQZ L72
! 				x[i] := j;
LDLW -4
LDLW 44
LDLW 12
LDLW 48
BOUND 326
STIW
! 				b[j] := FALSE;
CONST 0
LDLW 28
LDLW -4
LDLW 32
BOUND 327
STIC
! 				a[i+j] := FALSE;
CONST 0
LDLW 20
LDLW 12
LDLW -4
PLUS
LDLW 24
BOUND 328
STIC
! 				c[i-j+7] := FALSE;
CONST 0
LDLW 36
LDLW 12
LDLW -4
MINUS
CONST 7
PLUS
LDLW 40
BOUND 329
STIC
! 				IF i < 8 THEN
LDLW 12
CONST 8
JGEQ L80
! 					Try(i+1,q,a,b,c,x);
LDLW 48
LDLW 44
LDLW 40
LDLW 36
LDLW 32
LDLW 28
LDLW 24
LDLW 20
LDLW 16
LDLW 12
INC
GLOBAL tHennessy.Try
CALL 10
! 					IF ~q THEN
LDLW 16
LOADC
JNEQZ L72
! 						b[j] := TRUE;
CONST 1
LDLW 28
LDLW -4
LDLW 32
BOUND 333
STIC
! 						a[i+j] := TRUE;
CONST 1
LDLW 20
LDLW 12
LDLW -4
PLUS
LDLW 24
BOUND 334
STIC
! 						c[i-j+7] := TRUE
CONST 1
LDLW 36
LDLW 12
LDLW -4
MINUS
CONST 7
PLUS
LDLW 40
BOUND 335
STIC
JUMP L72
LABEL L80
!      	   	ELSE q := TRUE
CONST 1
LDLW 16
STOREC
JUMP L72
LABEL L74
RETURN
END

PROC tHennessy.Doit 84 11 0
! 	PROCEDURE Doit ();
! 		i := 0 - 7;
CONST -7
STLW -4
LABEL L87
! 		WHILE i <= 16 DO
LDLW -4
CONST 16
JGT L89
! 			IF (i >= 1) & (i <= 8) THEN a[i] := TRUE END ;
LDLW -4
CONST 1
JLT L92
LDLW -4
CONST 8
JGT L92
CONST 1
LOCAL -14
LDLW -4
CONST 9
BOUND 352
STIC
LABEL L92
! 			IF i >= 2 THEN b[i] := TRUE END ;
LDLW -4
CONST 2
JLT L96
CONST 1
LOCAL -31
LDLW -4
CONST 17
BOUND 353
STIC
LABEL L96
! 			IF i <= 7 THEN c[i+7] := TRUE END ;
LDLW -4
CONST 7
JGT L99
CONST 1
LOCAL -46
LDLW -4
CONST 7
PLUS
CONST 15
BOUND 354
STIC
LABEL L99
! 			i := i + 1;
INCL -4
JUMP L87
LABEL L89
! 		Try(1, q, b, a, c, x);
CONST 9
LOCAL -84
CONST 15
LOCAL -46
CONST 9
LOCAL -14
CONST 17
LOCAL -31
LOCAL -5
CONST 1
GLOBAL tHennessy.Try
CALL 10
! 		IF ( ~ q ) THEN Str(" Error in Queens.$") END
LDLC -5
JNEQZ L102
CONST 19
GLOBAL tHennessy.%6
GLOBAL tHennessy.Str
CALL 2
LABEL L102
RETURN
END

PROC tHennessy.Queens 4 3 0
! PROCEDURE Queens ();
! BEGIN i := 1;
CONST 1
STLW -4
LABEL L103
! 	WHILE i <= 50 DO Doit(); INC(i) END
LDLW -4
CONST 50
JGT L105
GLOBAL tHennessy.Doit
CALL 0
INCL -4
JUMP L103
LABEL L105
RETURN
END

PROC tHennessy.Initmatrix 12 5 0x00100001
! 	PROCEDURE Initmatrix (VAR m: intmatrix);
! 	BEGIN i := 1;
CONST 1
STLW -8
LABEL L106
! 		WHILE i <= rowsize DO
LDLW -8
CONST 40
JGT L108
! 			j := 1;
CONST 1
STLW -12
LABEL L109
! 			WHILE j <= rowsize DO
LDLW -12
CONST 40
JGT L111
! 				temp := Rand();
GLOBAL tHennessy.Rand
CALLW 0
STLW -4
! 				m[i][j] := temp - (temp DIV 120)*120 - 60;
LDLW -4
LDLW -4
CONST 120
DIV
CONST 120
TIMES
MINUS
CONST 60
MINUS
LDLW 12
LDLW -8
CONST 41
BOUND 377
CONST 41
TIMES
LDLW -12
CONST 41
BOUND 377
PLUS
STIW
! 				INC(j)
INCL -12
JUMP L109
LABEL L111
! 			INC(i)
INCL -8
JUMP L106
LABEL L108
RETURN
END

PROC tHennessy.Innerproduct 4 6 0x00700001
! 	PROCEDURE Innerproduct(VAR result: INTEGER; VAR a,b: intmatrix; row,column: INTEGER);
! 		result := 0; i := 1;
CONST 0
LDLW 12
STOREW
CONST 1
STLW -4
LABEL L112
! 		WHILE i <= rowsize DO result := result+a[row][i]*b[i][column]; INC(i) END
LDLW -4
CONST 40
JGT L114
LDLW 12
LOADW
LDLW 16
LDLW 24
CONST 41
BOUND 389
CONST 41
TIMES
LDLW -4
CONST 41
BOUND 389
PLUS
LDIW
LDLW 20
LDLW -4
CONST 41
BOUND 389
CONST 41
TIMES
LDLW 28
CONST 41
BOUND 389
PLUS
LDIW
TIMES
PLUS
LDLW 12
STOREW
INCL -4
JUMP L112
LABEL L114
RETURN
END

PROC tHennessy.Intmm 8 8 0
! PROCEDURE Intmm ();
! 	Initrand();
GLOBAL tHennessy.Initrand
CALL 0
! 	Initmatrix (ima);
GLOBAL tHennessy.ima
GLOBAL tHennessy.Initmatrix
CALL 1
! 	Initmatrix (imb);
GLOBAL tHennessy.imb
GLOBAL tHennessy.Initmatrix
CALL 1
! 	i := 1;
CONST 1
STLW -4
LABEL L115
! 	WHILE i <= rowsize DO j := 1;
LDLW -4
CONST 40
JGT L117
CONST 1
STLW -8
LABEL L118
! 		WHILE j <= rowsize DO Innerproduct(imr[i][j],ima,imb,i,j); INC(j) END ;
LDLW -8
CONST 40
JGT L120
LDLW -8
LDLW -4
GLOBAL tHennessy.imb
GLOBAL tHennessy.ima
GLOBAL tHennessy.imr
LDLW -4
CONST 41
BOUND 400
CONST 41
TIMES
LDLW -8
CONST 41
BOUND 400
PLUS
INDEXW
GLOBAL tHennessy.Innerproduct
CALL 5
INCL -8
JUMP L118
LABEL L120
! 		INC(i)
INCL -4
JUMP L115
LABEL L117
RETURN
END

PROC tHennessy.rInitmatrix 12 5 0x00100001
! 	PROCEDURE rInitmatrix (VAR m: realmatrix);
! 	BEGIN i := 1;
CONST 1
STLW -8
LABEL L121
! 		WHILE i <= rowsize DO j := 1;
LDLW -8
CONST 40
JGT L123
CONST 1
STLW -12
LABEL L124
! 			WHILE j <= rowsize DO
LDLW -12
CONST 40
JGT L126
! 				temp := Rand();
GLOBAL tHennessy.Rand
CALLW 0
STLW -4
! 				m[i][j] := (temp - (temp DIV 120)*120 - 60) DIV 3;
LDLW -4
LDLW -4
CONST 120
DIV
CONST 120
TIMES
MINUS
CONST 60
MINUS
CONST 3
DIV
CONVNF
LDLW 12
LDLW -8
CONST 41
BOUND 414
CONST 41
TIMES
LDLW -12
CONST 41
BOUND 414
PLUS
STIF
! 				INC(j)
INCL -12
JUMP L124
LABEL L126
! 			INC(i)
INCL -8
JUMP L121
LABEL L123
RETURN
END

PROC tHennessy.rInnerproduct 4 6 0x00700001
! 	PROCEDURE rInnerproduct(VAR result: REAL; VAR a,b: realmatrix; row,column: INTEGER);
! 		result := 0.0; i := 1;
FCONST 0.0
LDLW 12
STOREF
CONST 1
STLW -4
LABEL L127
! 		WHILE i<=rowsize DO result := result+a[row][i]*b[i][column]; INC(i) END
LDLW -4
CONST 40
JGT L129
LDLW 12
LOADF
LDLW 16
LDLW 24
CONST 41
BOUND 426
CONST 41
TIMES
LDLW -4
CONST 41
BOUND 426
PLUS
LDIF
LDLW 20
LDLW -4
CONST 41
BOUND 426
CONST 41
TIMES
LDLW 28
CONST 41
BOUND 426
PLUS
LDIF
FTIMES
FPLUS
LDLW 12
STOREF
INCL -4
JUMP L127
LABEL L129
RETURN
END

PROC tHennessy.Mm 8 8 0
! PROCEDURE Mm ();
! 	Initrand();
GLOBAL tHennessy.Initrand
CALL 0
! 	rInitmatrix (rma);
GLOBAL tHennessy.rma
GLOBAL tHennessy.rInitmatrix
CALL 1
! 	rInitmatrix (rmb);
GLOBAL tHennessy.rmb
GLOBAL tHennessy.rInitmatrix
CALL 1
! 	i := 1;
CONST 1
STLW -4
LABEL L130
! 	WHILE i <= rowsize DO j := 1;
LDLW -4
CONST 40
JGT L132
CONST 1
STLW -8
LABEL L133
! 		WHILE j <= rowsize DO rInnerproduct(rmr[i][j],rma,rmb,i,j); INC(j) END ;
LDLW -8
CONST 40
JGT L135
LDLW -8
LDLW -4
GLOBAL tHennessy.rmb
GLOBAL tHennessy.rma
GLOBAL tHennessy.rmr
LDLW -4
CONST 41
BOUND 437
CONST 41
TIMES
LDLW -8
CONST 41
BOUND 437
PLUS
INDEXW
GLOBAL tHennessy.rInnerproduct
CALL 5
INCL -8
JUMP L133
LABEL L135
! 		INC(i)
INCL -4
JUMP L130
LABEL L132
RETURN
END

PROC tHennessy.Fit 4 4 0
! 	PROCEDURE Fit (i, j: INTEGER): BOOLEAN;
! 	BEGIN k := 0;
CONST 0
STLW -4
LABEL L136
! 		WHILE k <= piecemax[i] DO
LDLW -4
GLOBAL tHennessy.piecemax
LDLW 12
CONST 13
BOUND 447
LDIW
JGT L138
! 			IF ( p[i][k] ) THEN IF ( puzzl[j+k] ) THEN RETURN FALSE END END;
GLOBAL tHennessy.p
LDLW 12
CONST 13
BOUND 448
CONST 512
TIMES
LDLW -4
CONST 512
BOUND 448
PLUS
LDIC
JEQZ L141
GLOBAL tHennessy.puzzl
LDLW 16
LDLW -4
PLUS
CONST 512
BOUND 448
LDIC
JEQZ L141
CONST 0
RETURN
LABEL L141
! 			INC(k)
INCL -4
JUMP L136
LABEL L138
! 		RETURN TRUE
CONST 1
RETURN
END

PROC tHennessy.Place 4 5 0
! 	PROCEDURE Place (i, j: INTEGER): INTEGER;
! 	BEGIN k := 0;
CONST 0
STLW -4
LABEL L145
! 		WHILE k <= piecemax[i] DO
LDLW -4
GLOBAL tHennessy.piecemax
LDLW 12
CONST 13
BOUND 457
LDIW
JGT L147
! 			IF ( p[i][k] ) THEN puzzl[j+k] := TRUE END;
GLOBAL tHennessy.p
LDLW 12
CONST 13
BOUND 458
CONST 512
TIMES
LDLW -4
CONST 512
BOUND 458
PLUS
LDIC
JEQZ L150
CONST 1
GLOBAL tHennessy.puzzl
LDLW 16
LDLW -4
PLUS
CONST 512
BOUND 458
STIC
LABEL L150
! 			INC(k)
INCL -4
JUMP L145
LABEL L147
! 		piececount[class[i]] := piececount[class[i]] - 1;
GLOBAL tHennessy.piececount
GLOBAL tHennessy.class
LDLW 12
CONST 13
BOUND 461
LDIW
CONST 4
BOUND 461
LDIW
DEC
GLOBAL tHennessy.piececount
GLOBAL tHennessy.class
LDLW 12
CONST 13
BOUND 461
LDIW
CONST 4
BOUND 461
STIW
! 		k := j;
LDLW 16
STLW -4
LABEL L151
! 		WHILE k <= size DO
LDLW -4
CONST 511
JGT L153
! 			IF ( ~ puzzl[k] ) THEN RETURN (k) END;
GLOBAL tHennessy.puzzl
LDLW -4
CONST 512
BOUND 464
LDIC
JNEQZ L156
LDLW -4
RETURN
LABEL L156
! 			INC(k)
INCL -4
JUMP L151
LABEL L153
! 		RETURN (0);
CONST 0
RETURN
END

PROC tHennessy.Remove 4 5 0
! 	PROCEDURE Remove (i, j: INTEGER);
! 	BEGIN k := 0;
CONST 0
STLW -4
LABEL L157
! 		WHILE k <= piecemax[i] DO
LDLW -4
GLOBAL tHennessy.piecemax
LDLW 12
CONST 13
BOUND 473
LDIW
JGT L159
! 			IF ( p[i][k] ) THEN puzzl[j+k] := FALSE END;
GLOBAL tHennessy.p
LDLW 12
CONST 13
BOUND 474
CONST 512
TIMES
LDLW -4
CONST 512
BOUND 474
PLUS
LDIC
JEQZ L162
CONST 0
GLOBAL tHennessy.puzzl
LDLW 16
LDLW -4
PLUS
CONST 512
BOUND 474
STIC
LABEL L162
! 			INC(k)
INCL -4
JUMP L157
LABEL L159
! 		piececount[class[i]] := piececount[class[i]] + 1
GLOBAL tHennessy.piececount
GLOBAL tHennessy.class
LDLW 12
CONST 13
BOUND 477
LDIW
CONST 4
BOUND 477
LDIW
INC
GLOBAL tHennessy.piececount
GLOBAL tHennessy.class
LDLW 12
CONST 13
BOUND 477
LDIW
CONST 4
BOUND 477
STIW
RETURN
END

PROC tHennessy.Trial 8 4 0
! 	PROCEDURE Trial (j: INTEGER): BOOLEAN;
! 	BEGIN i := 0;
CONST 0
STLW -4
! 		kount := kount + 1;
LDGW tHennessy.kount
INC
STGW tHennessy.kount
LABEL L163
! 		WHILE i <= typemax DO
LDLW -4
CONST 12
JGT L165
! 			IF ( piececount[class[i]] # 0 ) THEN
GLOBAL tHennessy.piececount
GLOBAL tHennessy.class
LDLW -4
CONST 13
BOUND 485
LDIW
CONST 4
BOUND 485
LDIW
JEQZ L168
! 				IF ( Fit (i, j) ) THEN
LDLW 12
LDLW -4
GLOBAL tHennessy.Fit
CALLW 2
JEQZ L168
! 					k := Place (i, j);
LDLW 12
LDLW -4
GLOBAL tHennessy.Place
CALLW 2
STLW -8
! 					IF Trial(k) OR (k = 0) THEN RETURN (TRUE)
LDLW -8
GLOBAL tHennessy.Trial
CALLW 1
JNEQZ L173
LDLW -8
JNEQZ L174
LABEL L173
CONST 1
RETURN
LABEL L174
! 					ELSE Remove (i, j)
LDLW 12
LDLW -4
GLOBAL tHennessy.Remove
CALL 2
LABEL L168
! 			INC(i)
INCL -4
JUMP L163
LABEL L165
! 		RETURN (FALSE)
CONST 0
RETURN
END

PROC tHennessy.Puzzle 16 7 0
! PROCEDURE Puzzle ();
! 	m := 0; WHILE m <= size DO puzzl[m] := TRUE; INC(m) END ;
CONST 0
STLW -16
LABEL L176
LDLW -16
CONST 511
JGT L178
CONST 1
GLOBAL tHennessy.puzzl
LDLW -16
CONST 512
BOUND 501
STIC
INCL -16
JUMP L176
LABEL L178
! 	i := 1;
CONST 1
STLW -4
LABEL L179
! 	WHILE i <= 5 DO j := 1;
LDLW -4
CONST 5
JGT L181
CONST 1
STLW -8
LABEL L182
! 		WHILE j <= 5 DO k := 1;
LDLW -8
CONST 5
JGT L184
CONST 1
STLW -12
LABEL L185
! 			WHILE k <= 5 DO
LDLW -12
CONST 5
JGT L187
! 				puzzl[i+d*(j+d*k)] := FALSE; INC(k)
CONST 0
GLOBAL tHennessy.puzzl
LDLW -4
LDLW -8
LDLW -12
CONST 8
TIMES
PLUS
CONST 8
TIMES
PLUS
CONST 512
BOUND 506
STIC
INCL -12
JUMP L185
LABEL L187
! 			INC(j)
INCL -8
JUMP L182
LABEL L184
! 		INC(i)
INCL -4
JUMP L179
LABEL L181
! 	i := 0; 
CONST 0
STLW -4
LABEL L188
! 	WHILE i <= typemax DO m := 0;
LDLW -4
CONST 12
JGT L190
CONST 0
STLW -16
LABEL L191
! 		WHILE m<= size DO
LDLW -16
CONST 511
JGT L193
! 			p[i][m] := FALSE; INC(m)
CONST 0
GLOBAL tHennessy.p
LDLW -4
CONST 13
BOUND 516
CONST 512
TIMES
LDLW -16
CONST 512
BOUND 516
PLUS
STIC
INCL -16
JUMP L191
LABEL L193
! 		INC(i)
INCL -4
JUMP L188
LABEL L190
! 	i := 0;
CONST 0
STLW -4
LABEL L194
! 	WHILE i <= 3 DO j := 0;
LDLW -4
CONST 3
JGT L196
CONST 0
STLW -8
LABEL L197
! 		WHILE j <= 1 DO k := 0;
LDLW -8
CONST 1
JGT L199
CONST 0
STLW -12
LABEL L200
! 			WHILE k <= 0 DO
LDLW -12
JGTZ L202
! 				p[0][i+d*(j+d*k)] := TRUE; INC(k)
CONST 1
GLOBAL tHennessy.p
CONST 0
LDLW -4
LDLW -8
LDLW -12
CONST 8
TIMES
PLUS
CONST 8
TIMES
PLUS
CONST 512
BOUND 525
PLUS
STIC
INCL -12
JUMP L200
LABEL L202
! 			INC(j)
INCL -8
JUMP L197
LABEL L199
! 		INC(i)
INCL -4
JUMP L194
LABEL L196
! 	class[0] := 0;
CONST 0
STGW tHennessy.class
! 	piecemax[0] := 3+d*1+d*d*0;
CONST 11
STGW tHennessy.piecemax
! 	i := 0;
CONST 0
STLW -4
LABEL L203
! 	WHILE i <= 1 DO j := 0;
LDLW -4
CONST 1
JGT L205
CONST 0
STLW -8
LABEL L206
! 		WHILE j <= 0 DO k := 0;
LDLW -8
JGTZ L208
CONST 0
STLW -12
LABEL L209
! 			WHILE k <= 3 DO
LDLW -12
CONST 3
JGT L211
! 				p[1][i+d*(j+d*k)] := TRUE; INC(k)
CONST 1
GLOBAL tHennessy.p
CONST 512
LDLW -4
LDLW -8
LDLW -12
CONST 8
TIMES
PLUS
CONST 8
TIMES
PLUS
CONST 512
BOUND 538
PLUS
STIC
INCL -12
JUMP L209
LABEL L211
! 			INC(j)
INCL -8
JUMP L206
LABEL L208
! 		INC(i)
INCL -4
JUMP L203
LABEL L205
! 	class[1] := 0;
CONST 0
GLOBAL tHennessy.class
STNW 4
! 	piecemax[1] := 1+d*0+d*d*3;
CONST 193
GLOBAL tHennessy.piecemax
STNW 4
! 	i := 0;
CONST 0
STLW -4
LABEL L212
! 	WHILE i <= 0 DO j := 0;
LDLW -4
JGTZ L214
CONST 0
STLW -8
LABEL L215
! 		WHILE j <= 3 DO k := 0;
LDLW -8
CONST 3
JGT L217
CONST 0
STLW -12
LABEL L218
! 			WHILE k <= 1 DO
LDLW -12
CONST 1
JGT L220
! 				p[2][i+d*(j+d*k)] := TRUE; INC(k)
CONST 1
GLOBAL tHennessy.p
CONST 1024
LDLW -4
LDLW -8
LDLW -12
CONST 8
TIMES
PLUS
CONST 8
TIMES
PLUS
CONST 512
BOUND 551
PLUS
STIC
INCL -12
JUMP L218
LABEL L220
! 			INC(j)
INCL -8
JUMP L215
LABEL L217
! 		INC(i)
INCL -4
JUMP L212
LABEL L214
! 	class[2] := 0;
CONST 0
GLOBAL tHennessy.class
STNW 8
! 	piecemax[2] := 0+d*3+d*d*1;
CONST 88
GLOBAL tHennessy.piecemax
STNW 8
! 	i := 0;
CONST 0
STLW -4
LABEL L221
! 	WHILE i <= 1 DO j := 0;
LDLW -4
CONST 1
JGT L223
CONST 0
STLW -8
LABEL L224
! 		WHILE j <= 3 DO k := 0;
LDLW -8
CONST 3
JGT L226
CONST 0
STLW -12
LABEL L227
! 			WHILE k <= 0 DO
LDLW -12
JGTZ L229
! 				p[3][i+d*(j+d*k)] := TRUE; INC(k)
CONST 1
GLOBAL tHennessy.p
CONST 1536
LDLW -4
LDLW -8
LDLW -12
CONST 8
TIMES
PLUS
CONST 8
TIMES
PLUS
CONST 512
BOUND 564
PLUS
STIC
INCL -12
JUMP L227
LABEL L229
! 			INC(j)
INCL -8
JUMP L224
LABEL L226
! 		INC(i)
INCL -4
JUMP L221
LABEL L223
! 	class[3] := 0;
CONST 0
GLOBAL tHennessy.class
STNW 12
! 	piecemax[3] := 1+d*3+d*d*0;
CONST 25
GLOBAL tHennessy.piecemax
STNW 12
! 	i := 0;
CONST 0
STLW -4
LABEL L230
! 	WHILE i <= 3 DO j := 0;
LDLW -4
CONST 3
JGT L232
CONST 0
STLW -8
LABEL L233
! 		WHILE j <= 0 DO k := 0;
LDLW -8
JGTZ L235
CONST 0
STLW -12
LABEL L236
! 			WHILE k <= 1 DO
LDLW -12
CONST 1
JGT L238
! 				p[4][i+d*(j+d*k)] := TRUE; INC(k)
CONST 1
GLOBAL tHennessy.p
CONST 2048
LDLW -4
LDLW -8
LDLW -12
CONST 8
TIMES
PLUS
CONST 8
TIMES
PLUS
CONST 512
BOUND 577
PLUS
STIC
INCL -12
JUMP L236
LABEL L238
! 			INC(j)
INCL -8
JUMP L233
LABEL L235
! 		INC(i)
INCL -4
JUMP L230
LABEL L232
! 	class[4] := 0;
CONST 0
GLOBAL tHennessy.class
STNW 16
! 	piecemax[4] := 3+d*0+d*d*1;
CONST 67
GLOBAL tHennessy.piecemax
STNW 16
! 	i := 0;
CONST 0
STLW -4
LABEL L239
! 	WHILE i <= 0 DO j := 0;
LDLW -4
JGTZ L241
CONST 0
STLW -8
LABEL L242
! 		WHILE j <= 1 DO k := 0;
LDLW -8
CONST 1
JGT L244
CONST 0
STLW -12
LABEL L245
! 			WHILE k <= 3 DO
LDLW -12
CONST 3
JGT L247
! 				p[5][i+d*(j+d*k)] := TRUE; INC(k)
CONST 1
GLOBAL tHennessy.p
CONST 2560
LDLW -4
LDLW -8
LDLW -12
CONST 8
TIMES
PLUS
CONST 8
TIMES
PLUS
CONST 512
BOUND 590
PLUS
STIC
INCL -12
JUMP L245
LABEL L247
! 			INC(j)
INCL -8
JUMP L242
LABEL L244
! 		INC(i)
INCL -4
JUMP L239
LABEL L241
! 	class[5] := 0;
CONST 0
GLOBAL tHennessy.class
STNW 20
! 	piecemax[5] := 0+d*1+d*d*3;
CONST 200
GLOBAL tHennessy.piecemax
STNW 20
! 	i := 0;
CONST 0
STLW -4
LABEL L248
! 	WHILE i <= 2 DO j := 0;
LDLW -4
CONST 2
JGT L250
CONST 0
STLW -8
LABEL L251
! 		WHILE j <= 0 DO k := 0;
LDLW -8
JGTZ L253
CONST 0
STLW -12
LABEL L254
! 			WHILE k <= 0 DO
LDLW -12
JGTZ L256
! 				p[6][i+d*(j+d*k)] := TRUE; INC(k)
CONST 1
GLOBAL tHennessy.p
CONST 3072
LDLW -4
LDLW -8
LDLW -12
CONST 8
TIMES
PLUS
CONST 8
TIMES
PLUS
CONST 512
BOUND 603
PLUS
STIC
INCL -12
JUMP L254
LABEL L256
! 			INC(j)
INCL -8
JUMP L251
LABEL L253
! 		INC(i)
INCL -4
JUMP L248
LABEL L250
! 	class[6] := 1;
CONST 1
GLOBAL tHennessy.class
STNW 24
! 	piecemax[6] := 2+d*0+d*d*0;
CONST 2
GLOBAL tHennessy.piecemax
STNW 24
! 	i := 0;
CONST 0
STLW -4
LABEL L257
! 	WHILE i <= 0 DO j := 0;
LDLW -4
JGTZ L259
CONST 0
STLW -8
LABEL L260
! 		WHILE j <= 2 DO k := 0;
LDLW -8
CONST 2
JGT L262
CONST 0
STLW -12
LABEL L263
! 			WHILE k <= 0 DO
LDLW -12
JGTZ L265
! 				p[7][i+d*(j+d*k)] := TRUE; INC(k)
CONST 1
GLOBAL tHennessy.p
CONST 3584
LDLW -4
LDLW -8
LDLW -12
CONST 8
TIMES
PLUS
CONST 8
TIMES
PLUS
CONST 512
BOUND 616
PLUS
STIC
INCL -12
JUMP L263
LABEL L265
! 			INC(j)
INCL -8
JUMP L260
LABEL L262
! 		INC(i)
INCL -4
JUMP L257
LABEL L259
! 	class[7] := 1;
CONST 1
GLOBAL tHennessy.class
STNW 28
! 	piecemax[7] := 0+d*2+d*d*0;
CONST 16
GLOBAL tHennessy.piecemax
STNW 28
! 	i := 0;
CONST 0
STLW -4
LABEL L266
! 	WHILE i <= 0 DO j := 0;
LDLW -4
JGTZ L268
CONST 0
STLW -8
LABEL L269
! 		WHILE j <= 0 DO k := 0;
LDLW -8
JGTZ L271
CONST 0
STLW -12
LABEL L272
! 			WHILE k <= 2 DO
LDLW -12
CONST 2
JGT L274
! 				p[8][i+d*(j+d*k)] := TRUE; INC(k)
CONST 1
GLOBAL tHennessy.p
CONST 4096
LDLW -4
LDLW -8
LDLW -12
CONST 8
TIMES
PLUS
CONST 8
TIMES
PLUS
CONST 512
BOUND 629
PLUS
STIC
INCL -12
JUMP L272
LABEL L274
! 			INC(j)
INCL -8
JUMP L269
LABEL L271
! 		INC(i)
INCL -4
JUMP L266
LABEL L268
! 	class[8] := 1;
CONST 1
GLOBAL tHennessy.class
STNW 32
!     piecemax[8] := 0+d*0+d*d*2;
CONST 128
GLOBAL tHennessy.piecemax
STNW 32
! 	i := 0;
CONST 0
STLW -4
LABEL L275
! 	WHILE i <= 1 DO j := 0;
LDLW -4
CONST 1
JGT L277
CONST 0
STLW -8
LABEL L278
! 		WHILE j <= 1 DO k := 0;
LDLW -8
CONST 1
JGT L280
CONST 0
STLW -12
LABEL L281
! 			WHILE k <= 0 DO
LDLW -12
JGTZ L283
! 				p[9][i+d*(j+d*k)] := TRUE; INC(k)
CONST 1
GLOBAL tHennessy.p
CONST 4608
LDLW -4
LDLW -8
LDLW -12
CONST 8
TIMES
PLUS
CONST 8
TIMES
PLUS
CONST 512
BOUND 642
PLUS
STIC
INCL -12
JUMP L281
LABEL L283
! 			INC(j)
INCL -8
JUMP L278
LABEL L280
! 		INC(i)
INCL -4
JUMP L275
LABEL L277
! 	class[9] := 2;
CONST 2
GLOBAL tHennessy.class
STNW 36
! 	piecemax[9] := 1+d*1+d*d*0;
CONST 9
GLOBAL tHennessy.piecemax
STNW 36
! 	i := 0;
CONST 0
STLW -4
LABEL L284
! 	WHILE i <= 1 DO j := 0;
LDLW -4
CONST 1
JGT L286
CONST 0
STLW -8
LABEL L287
! 		WHILE j <= 0 DO k := 0;
LDLW -8
JGTZ L289
CONST 0
STLW -12
LABEL L290
! 			WHILE k <= 1 DO
LDLW -12
CONST 1
JGT L292
! 				p[10][i+d*(j+d*k)] := TRUE; INC(k)
CONST 1
GLOBAL tHennessy.p
CONST 5120
LDLW -4
LDLW -8
LDLW -12
CONST 8
TIMES
PLUS
CONST 8
TIMES
PLUS
CONST 512
BOUND 655
PLUS
STIC
INCL -12
JUMP L290
LABEL L292
! 			INC(j)
INCL -8
JUMP L287
LABEL L289
! 		INC(i)
INCL -4
JUMP L284
LABEL L286
! 	class[10] := 2;
CONST 2
GLOBAL tHennessy.class
STNW 40
! 	piecemax[10] := 1+d*0+d*d*1;
CONST 65
GLOBAL tHennessy.piecemax
STNW 40
! 	i := 0;
CONST 0
STLW -4
LABEL L293
! 	WHILE i <= 0 DO j := 0;
LDLW -4
JGTZ L295
CONST 0
STLW -8
LABEL L296
! 		WHILE j <= 1 DO k := 0;
LDLW -8
CONST 1
JGT L298
CONST 0
STLW -12
LABEL L299
! 			WHILE k <= 1 DO
LDLW -12
CONST 1
JGT L301
! 				p[11][i+d*(j+d*k)] := TRUE; INC(k)
CONST 1
GLOBAL tHennessy.p
CONST 5632
LDLW -4
LDLW -8
LDLW -12
CONST 8
TIMES
PLUS
CONST 8
TIMES
PLUS
CONST 512
BOUND 668
PLUS
STIC
INCL -12
JUMP L299
LABEL L301
! 			INC(j)
INCL -8
JUMP L296
LABEL L298
! 		INC(i)
INCL -4
JUMP L293
LABEL L295
! 	class[11] := 2;
CONST 2
GLOBAL tHennessy.class
STNW 44
! 	piecemax[11] := 0+d*1+d*d*1;
CONST 72
GLOBAL tHennessy.piecemax
STNW 44
! 	i := 0;
CONST 0
STLW -4
LABEL L302
! 	WHILE i <= 1 DO j := 0;
LDLW -4
CONST 1
JGT L304
CONST 0
STLW -8
LABEL L305
! 		WHILE j <= 1 DO k := 0;
LDLW -8
CONST 1
JGT L307
CONST 0
STLW -12
LABEL L308
! 			WHILE k <= 1 DO
LDLW -12
CONST 1
JGT L310
! 				p[12][i+d*(j+d*k)] := TRUE; INC(k)
CONST 1
GLOBAL tHennessy.p
CONST 6144
LDLW -4
LDLW -8
LDLW -12
CONST 8
TIMES
PLUS
CONST 8
TIMES
PLUS
CONST 512
BOUND 681
PLUS
STIC
INCL -12
JUMP L308
LABEL L310
! 			INC(j)
INCL -8
JUMP L305
LABEL L307
! 		INC(i)
INCL -4
JUMP L302
LABEL L304
! 	class[12] := 3;
CONST 3
GLOBAL tHennessy.class
STNW 48
! 	piecemax[12] := 1+d*1+d*d*1;
CONST 73
GLOBAL tHennessy.piecemax
STNW 48
! 	piececount[0] := 13;
CONST 13
STGW tHennessy.piececount
! 	piececount[1] := 3;
CONST 3
GLOBAL tHennessy.piececount
STNW 4
! 	piececount[2] := 1;
CONST 1
GLOBAL tHennessy.piececount
STNW 8
! 	piececount[3] := 1;
CONST 1
GLOBAL tHennessy.piececount
STNW 12
! 	m := 1+d*(1+d*1);
CONST 73
STLW -16
! 	kount := 0;
CONST 0
STGW tHennessy.kount
! 	IF Fit(0, m) THEN n := Place(0, m)
LDLW -16
CONST 0
GLOBAL tHennessy.Fit
CALLW 2
JEQZ L313
LDLW -16
CONST 0
GLOBAL tHennessy.Place
CALLW 2
STGW tHennessy.n
JUMP L311
LABEL L313
! 	ELSE Str("Error1 in Puzzle$")
CONST 18
GLOBAL tHennessy.%7
GLOBAL tHennessy.Str
CALL 2
LABEL L311
! 	IF ~ Trial(n) THEN Str("Error2 in Puzzle.$")
LDGW tHennessy.n
GLOBAL tHennessy.Trial
CALLW 1
JNEQZ L316
CONST 19
GLOBAL tHennessy.%8
GLOBAL tHennessy.Str
CALL 2
RETURN
LABEL L316
! 	ELSIF kount # 2005 THEN Str("Error3 in Puzzle.$")
LDGW tHennessy.kount
CONST 2005
JEQ L318
CONST 19
GLOBAL tHennessy.%9
GLOBAL tHennessy.Str
CALL 2
LABEL L318
RETURN
END

PROC tHennessy.Initarr 8 4 0
! 	PROCEDURE Initarr();
! 		Initrand();
GLOBAL tHennessy.Initrand
CALL 0
! 		biggest := 0; littlest := 0; i := 1;
CONST 0
STGW tHennessy.biggest
CONST 0
STGW tHennessy.littlest
CONST 1
STLW -4
LABEL L319
! 		WHILE i <= sortelements DO
LDLW -4
CONST 5000
JGT L321
! 			temp := Rand();
GLOBAL tHennessy.Rand
CALLW 0
STLW -8
! 			sortlist[i] := temp - (temp DIV 100000)*100000 - 50000;
LDLW -8
LDLW -8
CONST 100000
DIV
CONST 100000
TIMES
MINUS
CONST 50000
MINUS
GLOBAL tHennessy.sortlist
LDLW -4
CONST 5001
BOUND 714
STIW
! 			IF sortlist[i] > biggest THEN biggest := sortlist[i]
GLOBAL tHennessy.sortlist
LDLW -4
CONST 5001
BOUND 715
LDIW
LDGW tHennessy.biggest
JLEQ L324
GLOBAL tHennessy.sortlist
LDLW -4
CONST 5001
BOUND 715
LDIW
STGW tHennessy.biggest
JUMP L326
LABEL L324
! 			ELSIF sortlist[i] < littlest THEN littlest := sortlist[i]
GLOBAL tHennessy.sortlist
LDLW -4
CONST 5001
BOUND 716
LDIW
LDGW tHennessy.littlest
JGEQ L326
GLOBAL tHennessy.sortlist
LDLW -4
CONST 5001
BOUND 716
LDIW
STGW tHennessy.littlest
LABEL L326
! 			INC(i)
INCL -4
JUMP L319
LABEL L321
RETURN
END

PROC tHennessy.Quicksort 16 5 0x00100001
! 	PROCEDURE Quicksort(VAR a: ARRAY OF INTEGER; l,r: INTEGER);
! 		i:=l; j:=r;
LDLW 20
STLW -4
LDLW 24
STLW -8
! 		x:=a[(l+r) DIV 2];
LDLW 12
LDLW 20
LDLW 24
PLUS
CONST 2
DIV
LDLW 16
BOUND 727
LDIW
STLW -12
LABEL L327
! 			WHILE a[i]<x DO i := i+1 END;
LDLW 12
LDLW -4
LDLW 16
BOUND 729
LDIW
LDLW -12
JGEQ L331
INCL -4
JUMP L327
LABEL L331
! 			WHILE x<a[j] DO j := j-1 END;
LDLW -12
LDLW 12
LDLW -8
LDLW 16
BOUND 730
LDIW
JGEQ L334
DECL -8
JUMP L331
LABEL L334
! 			IF i<=j THEN
LDLW -4
LDLW -8
JGT L337
! 				w := a[i];
LDLW 12
LDLW -4
LDLW 16
BOUND 732
LDIW
STLW -16
! 				a[i] := a[j];
LDLW 12
LDLW -8
LDLW 16
BOUND 733
LDIW
LDLW 12
LDLW -4
LDLW 16
BOUND 733
STIW
! 				a[j] := w;
LDLW -16
LDLW 12
LDLW -8
LDLW 16
BOUND 734
STIW
! 				i := i+1;    j := j-1
INCL -4
DECL -8
LABEL L337
! 		UNTIL i > j;
LDLW -4
LDLW -8
JLEQ L327
! 		IF l<j THEN Quicksort(a,l,j) END;
LDLW 20
LDLW -8
JGEQ L340
LDLW -8
LDLW 20
LDLW 16
LDLW 12
GLOBAL tHennessy.Quicksort
CALL 4
LABEL L340
! 		IF i<r THEN Quicksort(a,i,r) END
LDLW -4
LDLW 24
JGEQ L343
LDLW 24
LDLW -4
LDLW 16
LDLW 12
GLOBAL tHennessy.Quicksort
CALL 4
LABEL L343
RETURN
END

PROC tHennessy.Quick 0 5 0
! PROCEDURE Quick ();
!     Initarr();
GLOBAL tHennessy.Initarr
CALL 0
!     Quicksort(sortlist,1,sortelements);
CONST 5000
CONST 1
CONST 5001
GLOBAL tHennessy.sortlist
GLOBAL tHennessy.Quicksort
CALL 4
!     IF (sortlist[1] # littlest) OR (sortlist[sortelements] # biggest) THEN  Str( " Error in Quick.$") END ;
GLOBAL tHennessy.sortlist
LDNW 4
LDGW tHennessy.littlest
JNEQ L345
GLOBAL tHennessy.sortlist
LDNW 20000
LDGW tHennessy.biggest
JEQ L346
LABEL L345
CONST 18
GLOBAL tHennessy.%10
GLOBAL tHennessy.Str
CALL 2
LABEL L346
RETURN
END

PROC tHennessy.bInitarr 8 4 0
! 	PROCEDURE bInitarr();
! 		Initrand();
GLOBAL tHennessy.Initrand
CALL 0
! 		biggest := 0; littlest := 0; i := 1;
CONST 0
STGW tHennessy.biggest
CONST 0
STGW tHennessy.littlest
CONST 1
STLW -4
LABEL L348
! 		WHILE i <= srtelements DO
LDLW -4
CONST 500
JGT L350
! 			temp := Rand();
GLOBAL tHennessy.Rand
CALLW 0
STLW -8
! 			sortlist[i] := temp - (temp DIV 100000)*100000 - 50000;
LDLW -8
LDLW -8
CONST 100000
DIV
CONST 100000
TIMES
MINUS
CONST 50000
MINUS
GLOBAL tHennessy.sortlist
LDLW -4
CONST 5001
BOUND 759
STIW
! 			IF sortlist[i] > biggest THEN biggest := sortlist[i]
GLOBAL tHennessy.sortlist
LDLW -4
CONST 5001
BOUND 760
LDIW
LDGW tHennessy.biggest
JLEQ L353
GLOBAL tHennessy.sortlist
LDLW -4
CONST 5001
BOUND 760
LDIW
STGW tHennessy.biggest
JUMP L355
LABEL L353
! 			ELSIF sortlist[i] < littlest THEN littlest := sortlist[i]
GLOBAL tHennessy.sortlist
LDLW -4
CONST 5001
BOUND 761
LDIW
LDGW tHennessy.littlest
JGEQ L355
GLOBAL tHennessy.sortlist
LDLW -4
CONST 5001
BOUND 761
LDIW
STGW tHennessy.littlest
LABEL L355
! 			INC(i)
INCL -4
JUMP L348
LABEL L350
RETURN
END

PROC tHennessy.Bubble 8 4 0
! PROCEDURE Bubble();
! 	bInitarr();
GLOBAL tHennessy.bInitarr
CALL 0
! 	top:=srtelements;
CONST 500
STGW tHennessy.top
LABEL L356
! 	WHILE top>1 DO
LDGW tHennessy.top
CONST 1
JLEQ L358
! 		i:=1;
CONST 1
STLW -4
LABEL L359
! 		WHILE i<top DO
LDLW -4
LDGW tHennessy.top
JGEQ L361
! 			IF sortlist[i] > sortlist[i+1] THEN
GLOBAL tHennessy.sortlist
LDLW -4
CONST 5001
BOUND 775
LDIW
GLOBAL tHennessy.sortlist
LDLW -4
INC
CONST 5001
BOUND 775
LDIW
JLEQ L364
! 				j := sortlist[i];
GLOBAL tHennessy.sortlist
LDLW -4
CONST 5001
BOUND 776
LDIW
STLW -8
! 				sortlist[i] := sortlist[i+1];
GLOBAL tHennessy.sortlist
LDLW -4
INC
CONST 5001
BOUND 777
LDIW
GLOBAL tHennessy.sortlist
LDLW -4
CONST 5001
BOUND 777
STIW
! 				sortlist[i+1] := j;
LDLW -8
GLOBAL tHennessy.sortlist
LDLW -4
INC
CONST 5001
BOUND 778
STIW
LABEL L364
! 			i:=i+1;
INCL -4
JUMP L359
LABEL L361
! 		top:=top-1;
LDGW tHennessy.top
DEC
STGW tHennessy.top
JUMP L356
LABEL L358
! 	IF (sortlist[1] # littlest) OR (sortlist[srtelements] # biggest) THEN Str("Error3 in Bubble.$") END ;
GLOBAL tHennessy.sortlist
LDNW 4
LDGW tHennessy.littlest
JNEQ L366
GLOBAL tHennessy.sortlist
LDNW 2000
LDGW tHennessy.biggest
JEQ L367
LABEL L366
CONST 19
GLOBAL tHennessy.%11
GLOBAL tHennessy.Str
CALL 2
LABEL L367
RETURN
END

PROC tHennessy.tInitarr 8 4 0
! 	PROCEDURE tInitarr();
! 		Initrand();
GLOBAL tHennessy.Initrand
CALL 0
! 		biggest := 0; littlest := 0; i := 1;
CONST 0
STGW tHennessy.biggest
CONST 0
STGW tHennessy.littlest
CONST 1
STLW -4
LABEL L369
! 		WHILE i <= sortelements DO
LDLW -4
CONST 5000
JGT L371
! 			temp := Rand();
GLOBAL tHennessy.Rand
CALLW 0
STLW -8
! 			sortlist[i] := temp - (temp DIV 100000)*100000 - 50000;
LDLW -8
LDLW -8
CONST 100000
DIV
CONST 100000
TIMES
MINUS
CONST 50000
MINUS
GLOBAL tHennessy.sortlist
LDLW -4
CONST 5001
BOUND 796
STIW
! 			IF sortlist[i] > biggest THEN biggest := sortlist[i]
GLOBAL tHennessy.sortlist
LDLW -4
CONST 5001
BOUND 797
LDIW
LDGW tHennessy.biggest
JLEQ L374
GLOBAL tHennessy.sortlist
LDLW -4
CONST 5001
BOUND 797
LDIW
STGW tHennessy.biggest
JUMP L376
LABEL L374
! 			ELSIF sortlist[i] < littlest THEN littlest := sortlist[i]
GLOBAL tHennessy.sortlist
LDLW -4
CONST 5001
BOUND 798
LDIW
LDGW tHennessy.littlest
JGEQ L376
GLOBAL tHennessy.sortlist
LDLW -4
CONST 5001
BOUND 798
LDIW
STGW tHennessy.littlest
LABEL L376
! 			INC(i)
INCL -4
JUMP L369
LABEL L371
RETURN
END

PROC tHennessy.CreateNode 0 3 0x00100001
! 	PROCEDURE CreateNode (VAR t: node; n: INTEGER);
!     	NEW(t);
CONST 12
GLOBAL tHennessy.nodeDesc
GLOBAL NEW
CALLW 2
LDLW 12
STOREW
! 		t.left := NIL; t.right := NIL;
CONST 0
LDLW 12
LOADW
NCHECK 807
STOREW
CONST 0
LDLW 12
LOADW
NCHECK 807
STNW 4
! 		t.val := n
LDLW 16
LDLW 12
LOADW
NCHECK 808
STNW 8
RETURN
END

PROC tHennessy.Insert 0 3 0x00200001
! 	PROCEDURE Insert(n: INTEGER; t: node);
! 		IF n > t.val THEN
LDLW 12
LDLW 16
NCHECK 814
LDNW 8
JLEQ L379
! 			IF t.left = NIL THEN CreateNode(t.left,n)
LDLW 16
NCHECK 815
LOADW
JNEQZ L382
LDLW 12
LDLW 16
NCHECK 815
GLOBAL tHennessy.CreateNode
CALL 2
RETURN
LABEL L382
! 			ELSE Insert(n,t.left)
LDLW 16
NCHECK 816
LOADW
LDLW 12
GLOBAL tHennessy.Insert
CALL 2
RETURN
LABEL L379
! 		ELSIF n < t.val THEN
LDLW 12
LDLW 16
NCHECK 818
LDNW 8
JGEQ L384
! 			IF t.right = NIL THEN CreateNode(t.right,n)
LDLW 16
NCHECK 819
LDNW 4
JNEQZ L387
LDLW 12
LDLW 16
NCHECK 819
ADJUST 4
GLOBAL tHennessy.CreateNode
CALL 2
RETURN
LABEL L387
! 			ELSE Insert(n,t.right)
LDLW 16
NCHECK 820
LDNW 4
LDLW 12
GLOBAL tHennessy.Insert
CALL 2
LABEL L384
RETURN
END

PROC tHennessy.Checktree 4 2 0x00100001
! 	PROCEDURE Checktree(p: node): BOOLEAN;
! 		result := TRUE;
CONST 1
STLC -1
! 		IF p.left # NIL THEN
LDLW 12
NCHECK 830
LOADW
JEQZ L390
! 			IF p.left.val <= p.val THEN result := FALSE;
LDLW 12
NCHECK 831
LOADW
NCHECK 831
LDNW 8
LDLW 12
NCHECK 831
LDNW 8
JGT L393
CONST 0
STLC -1
JUMP L390
LABEL L393
! 			ELSE result := Checktree(p.left) & result
LDLW 12
NCHECK 832
LOADW
GLOBAL tHennessy.Checktree
CALLW 1
LDLC -1
AND
STLC -1
LABEL L390
! 		IF  p.right # NIL THEN
LDLW 12
NCHECK 835
LDNW 4
JEQZ L396
! 			IF p.right.val >= p.val THEN result := FALSE;
LDLW 12
NCHECK 836
LDNW 4
NCHECK 836
LDNW 8
LDLW 12
NCHECK 836
LDNW 8
JLT L399
CONST 0
STLC -1
JUMP L396
LABEL L399
! 			ELSE result := Checktree(p.right) & result
LDLW 12
NCHECK 837
LDNW 4
GLOBAL tHennessy.Checktree
CALLW 1
LDLC -1
AND
STLC -1
LABEL L396
! 		RETURN result
LDLC -1
RETURN
END

PROC tHennessy.Trees 4 4 0
! PROCEDURE Trees();
! 	tInitarr();
GLOBAL tHennessy.tInitarr
CALL 0
! 	NEW(tree);
CONST 12
GLOBAL tHennessy.nodeDesc
GLOBAL NEW
CALLW 2
STGW tHennessy.tree
! 	tree.left := NIL; tree.right:=NIL; tree.val:=sortlist[1];
CONST 0
LDGW tHennessy.tree
NCHECK 848
STOREW
CONST 0
LDGW tHennessy.tree
NCHECK 848
STNW 4
GLOBAL tHennessy.sortlist
LDNW 4
LDGW tHennessy.tree
NCHECK 848
STNW 8
! 	i := 2;
CONST 2
STLW -4
LABEL L400
!     WHILE i <= sortelements DO
LDLW -4
CONST 5000
JGT L402
!     	Insert(sortlist[i],tree);
LDGW tHennessy.tree
GLOBAL tHennessy.sortlist
LDLW -4
CONST 5001
BOUND 851
LDIW
GLOBAL tHennessy.Insert
CALL 2
!     	INC(i)
INCL -4
JUMP L400
LABEL L402
! 	IF ~ Checktree(tree) THEN Str(" Error in Tree.$") END;
LDGW tHennessy.tree
GLOBAL tHennessy.Checktree
CALLW 1
JNEQZ L405
CONST 17
GLOBAL tHennessy.%12
GLOBAL tHennessy.Str
CALL 2
LABEL L405
RETURN
END

PROC tHennessy.Cos 16 3 0
! 	PROCEDURE Cos (x: REAL): REAL;
! 		result := 1.0; factor := 1;  power := x; i := 2;
FCONST 1.0
STLF -12
CONST 1
STLW -8
LDLF 12
STLF -16
CONST 2
STLW -4
LABEL L406
! 		WHILE i <= 10 DO
LDLW -4
CONST 10
JGT L408
! 			factor := factor * i;  power := power*x;
LDLW -8
LDLW -4
TIMES
STLW -8
LDLF -16
LDLF 12
FTIMES
STLF -16
! 			IF i MOD 2 = 0 THEN
LDLW -4
CONST 2
MOD
JNEQZ L411
! 				IF i MOD 4 = 0 THEN result := result + power/factor
LDLW -4
CONST 4
MOD
JNEQZ L414
LDLF -12
LDLF -16
LDLW -8
CONVNF
FZCHECK 866
FDIV
FPLUS
STLF -12
JUMP L411
LABEL L414
! 				ELSE result := result - power/factor
LDLF -12
LDLF -16
LDLW -8
CONVNF
FZCHECK 867
FDIV
FMINUS
STLF -12
LABEL L411
! 			INC(i)
INCL -4
JUMP L406
LABEL L408
! 		RETURN result
LDLF -12
RETURN
END

PROC tHennessy.Min0 0 2 0
! 	PROCEDURE Min0( arg1, arg2: INTEGER): INTEGER;
! 		IF arg1 < arg2 THEN RETURN arg1
LDLW 12
LDLW 16
JGEQ L417
LDLW 12
RETURN
LABEL L417
! 		ELSE RETURN arg2
LDLW 16
RETURN
END

PROC tHennessy.Uniform11 0 2 0
! 	PROCEDURE Uniform11(iy: INTEGER; yfl: REAL);
! 		iy := (4855*iy + 1731) MOD 8192;
LDLW 12
CONST 4855
TIMES
CONST 1731
PLUS
CONST 8192
MOD
STLW 12
! 		yfl := iy/8192.0;
LDLW 12
CONVNF
FCONST 8192.0
FDIV
STLF 16
RETURN
END

PROC tHennessy.Exptab 144 4 0x00200001
! 	PROCEDURE Exptab(n: INTEGER; VAR e: c2array);
! 		theta := 3.1415926536;
FCONST 3.1415926536
STLF -4
! 		divisor := 4.0; i:=1;
FCONST 4.0
STLF -8
CONST 1
STLW -120
LABEL L418
! 		WHILE i <= 25 DO
LDLW -120
CONST 25
JGT L420
! 			h[i] := 1/(2*Cos( theta/divisor ));
FCONST 1.0
LDLF -4
LDLF -8
FZCHECK 895
FDIV
GLOBAL tHennessy.Cos
CALLF 1
FCONST 2.0
FTIMES
FZCHECK 895
FDIV
LOCAL -116
LDLW -120
CONST 26
BOUND 895
STIF
! 			divisor := divisor + divisor;
LDLF -8
LDLF -8
FPLUS
STLF -8
! 			INC(i)
INCL -120
JUMP L418
LABEL L420
! 		m := n DIV 2 ;
LDLW 12
CONST 2
DIV
STLW -136
! 		l := m DIV 2 ;
LDLW -136
CONST 2
DIV
STLW -132
! 		j := 1 ;
CONST 1
STLW -124
! 		e[1].rp := 1.0 ;
FCONST 1.0
LDLW 16
STNF 8
! 		e[1].ip := 0.0;
FCONST 0.0
LDLW 16
STNF 12
! 		e[l+1].rp := 0.0;
FCONST 0.0
LDLW 16
LDLW -132
INC
CONST 130
BOUND 904
INDEXD
STOREF
! 		e[l+1].ip := 1.0 ;
FCONST 1.0
LDLW 16
LDLW -132
INC
CONST 130
BOUND 905
INDEXD
STNF 4
! 		e[m+1].rp := -1.0 ;
FCONST -1.0
LDLW 16
LDLW -136
INC
CONST 130
BOUND 906
INDEXD
STOREF
! 		e[m+1].ip := 0.0 ;
FCONST 0.0
LDLW 16
LDLW -136
INC
CONST 130
BOUND 907
INDEXD
STNF 4
LABEL L421
! 			i := l DIV 2 ;
LDLW -132
CONST 2
DIV
STLW -120
! 			k := i ;
LDLW -120
STLW -128
LABEL L423
! 				d1:= k+i+1; d2:= k-i+1; d3:= e[ d2].rp;
LDLW -128
LDLW -120
PLUS
INC
STLW -140
LDLW -128
LDLW -120
MINUS
INC
STLW -144
LDLW 16
LDLW -144
CONST 130
BOUND 912
INDEXD
LOADF
STLF -12
! 				e[k+1].rp := h[j]*(e[ d1].rp+d3) ; d3:= e[ d2].ip;
LOCAL -116
LDLW -124
CONST 26
BOUND 913
LDIF
LDLW 16
LDLW -140
CONST 130
BOUND 913
INDEXD
LOADF
LDLF -12
FPLUS
FTIMES
LDLW 16
LDLW -128
INC
CONST 130
BOUND 913
INDEXD
STOREF
LDLW 16
LDLW -144
CONST 130
BOUND 913
INDEXD
LDNF 4
STLF -12
! 				e[k+1].ip := h[j]*(e[ d1].ip+d3) ;
LOCAL -116
LDLW -124
CONST 26
BOUND 914
LDIF
LDLW 16
LDLW -140
CONST 130
BOUND 914
INDEXD
LDNF 4
LDLF -12
FPLUS
FTIMES
LDLW 16
LDLW -128
INC
CONST 130
BOUND 914
INDEXD
STNF 4
! 				k := k+l ;
LDLW -128
LDLW -132
PLUS
STLW -128
! 			UNTIL ( k > m );
LDLW -128
LDLW -136
JLEQ L423
! 			j := Min0( j+1, 25);
CONST 25
LDLW -124
INC
GLOBAL tHennessy.Min0
CALLW 2
STLW -124
! 			l := i ;
LDLW -120
STLW -132
! 		UNTIL ( l <= 1 );
LDLW -132
CONST 1
JGT L421
RETURN
END

PROC tHennessy.Fft 32 5 0x00e00001
! 	PROCEDURE Fft( n: INTEGER; VAR z, w: carray; VAR e: c2array; sqrinv: REAL);
! 		m := n DIV 2 ;
LDLW 12
CONST 2
DIV
STLW -20
! 		l := 1 ;
CONST 1
STLW -16
LABEL L425
! 			k := 0 ;
CONST 0
STLW -12
! 			j := l ;
LDLW -16
STLW -8
! 			i := 1 ;
CONST 1
STLW -4
LABEL L427
! 					w[i+k].rp := z[i].rp+z[m+i].rp ;
LDLW 16
LDLW -4
CONST 257
BOUND 933
INDEXD
LOADF
LDLW 16
LDLW -20
LDLW -4
PLUS
CONST 257
BOUND 933
INDEXD
LOADF
FPLUS
LDLW 20
LDLW -4
LDLW -12
PLUS
CONST 257
BOUND 933
INDEXD
STOREF
! 					w[i+k].ip := z[i].ip+z[m+i].ip ;
LDLW 16
LDLW -4
CONST 257
BOUND 934
INDEXD
LDNF 4
LDLW 16
LDLW -20
LDLW -4
PLUS
CONST 257
BOUND 934
INDEXD
LDNF 4
FPLUS
LDLW 20
LDLW -4
LDLW -12
PLUS
CONST 257
BOUND 934
INDEXD
STNF 4
! 					h := e[k+1].rp*(z[i].rp-z[i+m].rp);	d1:= (z[i].ip-z[i+m].ip);
LDLW 24
LDLW -12
INC
CONST 130
BOUND 935
INDEXD
LOADF
LDLW 16
LDLW -4
CONST 257
BOUND 935
INDEXD
LOADF
LDLW 16
LDLW -4
LDLW -20
PLUS
CONST 257
BOUND 935
INDEXD
LOADF
FMINUS
FTIMES
STLF -28
LDLW 16
LDLW -4
CONST 257
BOUND 935
INDEXD
LDNF 4
LDLW 16
LDLW -4
LDLW -20
PLUS
CONST 257
BOUND 935
INDEXD
LDNF 4
FMINUS
STLF -32
! 					w[i+j].rp := h-e[k+1].ip*d1 ;
LDLF -28
LDLW 24
LDLW -12
INC
CONST 130
BOUND 936
INDEXD
LDNF 4
LDLF -32
FTIMES
FMINUS
LDLW 20
LDLW -4
LDLW -8
PLUS
CONST 257
BOUND 936
INDEXD
STOREF
! 					h := e[k+1].rp*(z[i].ip-z[i+m].ip);	d1:=(z[i].rp-z[i+m].rp);
LDLW 24
LDLW -12
INC
CONST 130
BOUND 937
INDEXD
LOADF
LDLW 16
LDLW -4
CONST 257
BOUND 937
INDEXD
LDNF 4
LDLW 16
LDLW -4
LDLW -20
PLUS
CONST 257
BOUND 937
INDEXD
LDNF 4
FMINUS
FTIMES
STLF -28
LDLW 16
LDLW -4
CONST 257
BOUND 937
INDEXD
LOADF
LDLW 16
LDLW -4
LDLW -20
PLUS
CONST 257
BOUND 937
INDEXD
LOADF
FMINUS
STLF -32
! 					w[i+j].ip := h+e[k+1].ip*d1 ;
LDLF -28
LDLW 24
LDLW -12
INC
CONST 130
BOUND 938
INDEXD
LDNF 4
LDLF -32
FTIMES
FPLUS
LDLW 20
LDLW -4
LDLW -8
PLUS
CONST 257
BOUND 938
INDEXD
STNF 4
! 					i := i+1 ;
INCL -4
! 				UNTIL ( i > j );
LDLW -4
LDLW -8
JLEQ L427
! 				k := j ;
LDLW -8
STLW -12
! 				j := k+l ;
LDLW -12
LDLW -16
PLUS
STLW -8
! 			UNTIL ( j > m );
LDLW -8
LDLW -20
JLEQ L427
! 			(*z := w ;*) index := 1;
CONST 1
STLW -24
LABEL L431
! 				z[index] := w[index];
LDLW 16
LDLW -24
CONST 257
BOUND 946
INDEXD
LDLW 20
LDLW -24
CONST 257
BOUND 946
INDEXD
CONST 8
FIXCOPY
! 				index := index+1;
INCL -24
! 			UNTIL ( index > n );
LDLW -24
LDLW 12
JLEQ L431
! 			l := l+l ;
LDLW -16
LDLW -16
PLUS
STLW -16
! 		UNTIL ( l > m );
LDLW -16
LDLW -20
JLEQ L425
! 		i := 1;
CONST 1
STLW -4
LABEL L433
! 		WHILE i <= n DO
LDLW -4
LDLW 12
JGT L435
! 			z[i].rp := sqrinv*z[i].rp ;
LDLF 28
LDLW 16
LDLW -4
CONST 257
BOUND 953
INDEXD
LOADF
FTIMES
LDLW 16
LDLW -4
CONST 257
BOUND 953
INDEXD
STOREF
! 			z[i].ip := -sqrinv*z[i].ip;
LDLF 28
LDLW 16
LDLW -4
CONST 257
BOUND 954
INDEXD
LDNF 4
FTIMES
FUMINUS
LDLW 16
LDLW -4
CONST 257
BOUND 954
INDEXD
STNF 4
! 			INC(i)
INCL -4
JUMP L433
LABEL L435
RETURN
END

PROC tHennessy.Oscar 4 6 0
! PROCEDURE Oscar ();
! 	Exptab(fftsize,e) ;
GLOBAL tHennessy.e
CONST 256
GLOBAL tHennessy.Exptab
CALL 2
! 	seed := 5767 ; i := 1;
CONST 5767
STGW tHennessy.seed
CONST 1
STLW -4
LABEL L436
! 	WHILE i <= fftsize DO
LDLW -4
CONST 256
JGT L438
! 		Uniform11( seed, zr );
LDGF tHennessy.zr
LDGW tHennessy.seed
GLOBAL tHennessy.Uniform11
CALL 2
! 		Uniform11( seed, zi );
LDGF tHennessy.zi
LDGW tHennessy.seed
GLOBAL tHennessy.Uniform11
CALL 2
! 		z[i].rp := 20.0*zr - 10.0;
LDGF tHennessy.zr
FCONST 20.0
FTIMES
FCONST 10.0
FMINUS
GLOBAL tHennessy.z
LDLW -4
CONST 257
BOUND 967
INDEXD
STOREF
! 		z[i].ip := 20.0*zi - 10.0;
LDGF tHennessy.zi
FCONST 20.0
FTIMES
FCONST 10.0
FMINUS
GLOBAL tHennessy.z
LDLW -4
CONST 257
BOUND 968
INDEXD
STNF 4
! 		INC(i)
INCL -4
JUMP L436
LABEL L438
! 	i := 1;
CONST 1
STLW -4
LABEL L439
! 	WHILE i <= 20 DO Fft(fftsize,z,w,e,0.0625); INC(i) END
LDLW -4
CONST 20
JGT L441
FCONST 0.0625
GLOBAL tHennessy.e
GLOBAL tHennessy.w
GLOBAL tHennessy.z
CONST 256
GLOBAL tHennessy.Fft
CALL 5
INCL -4
JUMP L439
LABEL L441
RETURN
END

PROC tHennessy.Time 8 3 0
! PROCEDURE Time(s: ARRAY OF CHAR; p: Proc; base, fbase: REAL);
LOCAL 12
LDLW 16
FLEXCOPY
! 	i := 0;
CONST 0
STLW -8
! 	timer := Getclock();
GLOBAL tHennessy.Getclock
CALLW 0
STLW -4
LABEL L442
! 	WHILE i < 10 DO p; INC(i) END ;
LDLW -8
CONST 10
JGEQ L444
LDLW 24
STATLINK
LDLW 20
NCHECK 983
CALL 0
INCL -8
JUMP L442
LABEL L444
! 	timer := (Getclock()-timer);
GLOBAL tHennessy.Getclock
CALLW 0
LDLW -4
MINUS
STLW -4
! 	fixed := fixed + timer*base;
LDGF tHennessy.fixed
LDLW -4
CONVNF
LDLF 28
FTIMES
FPLUS
STGF tHennessy.fixed
! 	floated := floated + timer*fbase
LDGF tHennessy.floated
LDLW -4
CONVNF
LDLF 32
FTIMES
FPLUS
STGF tHennessy.floated
RETURN
END

PROC tHennessy.Do 0 7 0
! PROCEDURE Do*;
! 	fixed := 0.0;  floated := 0.0;
FCONST 0.0
STGF tHennessy.fixed
FCONST 0.0
STGF tHennessy.floated
! 	Time("Perm ", Perm, permbase, permbase);
FCONST 1.75
FCONST 1.75
CONST 0
GLOBAL tHennessy.Perm
CONST 6
GLOBAL tHennessy.%13
GLOBAL tHennessy.Time
CALL 6
! 	Time("Towers ", Towers, towersbase, towersbase);
FCONST 2.39
FCONST 2.39
CONST 0
GLOBAL tHennessy.Towers
CONST 8
GLOBAL tHennessy.%14
GLOBAL tHennessy.Time
CALL 6
! 	Time("Queens ", Queens, queensbase, queensbase);
FCONST 1.83
FCONST 1.83
CONST 0
GLOBAL tHennessy.Queens
CONST 8
GLOBAL tHennessy.%15
GLOBAL tHennessy.Time
CALL 6
! 	Time("Intmm ", Intmm, intmmbase, intmmbase);
FCONST 1.46
FCONST 1.46
CONST 0
GLOBAL tHennessy.Intmm
CONST 7
GLOBAL tHennessy.%16
GLOBAL tHennessy.Time
CALL 6
! 	Time("Mm ", Mm, mmbase, fpmmbase);
FCONST 2.92
FCONST 0.0
CONST 0
GLOBAL tHennessy.Mm
CONST 4
GLOBAL tHennessy.%17
GLOBAL tHennessy.Time
CALL 6
! 	Time("Puzzle ", Puzzle, puzzlebase, puzzlebase);
FCONST 0.5
FCONST 0.5
CONST 0
GLOBAL tHennessy.Puzzle
CONST 8
GLOBAL tHennessy.%18
GLOBAL tHennessy.Time
CALL 6
! 	Time("Quick ", Quick, quickbase, quickbase);
FCONST 1.92
FCONST 1.92
CONST 0
GLOBAL tHennessy.Quick
CONST 7
GLOBAL tHennessy.%19
GLOBAL tHennessy.Time
CALL 6
! 	Time("Bubble ", Bubble, bubblebase, bubblebase);
FCONST 1.61
FCONST 1.61
CONST 0
GLOBAL tHennessy.Bubble
CONST 8
GLOBAL tHennessy.%20
GLOBAL tHennessy.Time
CALL 6
! 	Time("Tree ", Trees, treebase, treebase);
FCONST 2.5
FCONST 2.5
CONST 0
GLOBAL tHennessy.Trees
CONST 6
GLOBAL tHennessy.%21
GLOBAL tHennessy.Time
CALL 6
! 	Time("FFT ", Oscar, fftbase, fpfftbase);
FCONST 4.44
FCONST 0.0
CONST 0
GLOBAL tHennessy.Oscar
CONST 5
GLOBAL tHennessy.%22
GLOBAL tHennessy.Time
CALL 6
RETURN
END

PROC tHennessy.%main 0 3 0
! 	Do;
GLOBAL tHennessy.Do
CALL 0
!         Str("Done$")
CONST 6
GLOBAL tHennessy.%23
GLOBAL tHennessy.Str
CALL 2
RETURN
END

! Global variables
GLOVAR tHennessy.fixed 4
GLOVAR tHennessy.floated 4
GLOVAR tHennessy.seed 4
GLOVAR tHennessy.permarray 44
GLOVAR tHennessy.pctr 4
GLOVAR tHennessy.tree 4
GLOVAR tHennessy.stack 16
GLOVAR tHennessy.cellspace 152
GLOVAR tHennessy.freelist 4
GLOVAR tHennessy.movesdone 4
GLOVAR tHennessy.ima 6724
GLOVAR tHennessy.imb 6724
GLOVAR tHennessy.imr 6724
GLOVAR tHennessy.rma 6724
GLOVAR tHennessy.rmb 6724
GLOVAR tHennessy.rmr 6724
GLOVAR tHennessy.piececount 16
GLOVAR tHennessy.class 52
GLOVAR tHennessy.piecemax 52
GLOVAR tHennessy.puzzl 512
GLOVAR tHennessy.p 6656
GLOVAR tHennessy.n 4
GLOVAR tHennessy.kount 4
GLOVAR tHennessy.sortlist 20004
GLOVAR tHennessy.biggest 4
GLOVAR tHennessy.littlest 4
GLOVAR tHennessy.top 4
GLOVAR tHennessy.z 2056
GLOVAR tHennessy.w 2056
GLOVAR tHennessy.e 1040
GLOVAR tHennessy.zr 4
GLOVAR tHennessy.zi 4

! Global pointer map
DEFINE tHennessy.%gcmap
WORD GC_POINTER
WORD tHennessy.tree
WORD GC_END

! String " Error in Perm.$"
DEFINE tHennessy.%1
STRING 204572726F7220696E205065726D2E2400

! String "out of space   $"
DEFINE tHennessy.%2
STRING 6F7574206F662073706163652020202400

! String "disc size error$"
DEFINE tHennessy.%3
STRING 646973632073697A65206572726F722400

! String "nothing to pop $"
DEFINE tHennessy.%4
STRING 6E6F7468696E6720746F20706F70202400

! String " Error in Towers.$"
DEFINE tHennessy.%5
STRING 204572726F7220696E20546F776572732E2400

! String " Error in Queens.$"
DEFINE tHennessy.%6
STRING 204572726F7220696E20517565656E732E2400

! String "Error1 in Puzzle$"
DEFINE tHennessy.%7
STRING 4572726F723120696E2050757A7A6C652400

! String "Error2 in Puzzle.$"
DEFINE tHennessy.%8
STRING 4572726F723220696E2050757A7A6C652E2400

! String "Error3 in Puzzle.$"
DEFINE tHennessy.%9
STRING 4572726F723320696E2050757A7A6C652E2400

! String " Error in Quick.$"
DEFINE tHennessy.%10
STRING 204572726F7220696E20517569636B2E2400

! String "Error3 in Bubble.$"
DEFINE tHennessy.%11
STRING 4572726F723320696E20427562626C652E2400

! String " Error in Tree.$"
DEFINE tHennessy.%12
STRING 204572726F7220696E20547265652E2400

! String "Perm "
DEFINE tHennessy.%13
STRING 5065726D2000

! String "Towers "
DEFINE tHennessy.%14
STRING 546F776572732000

! String "Queens "
DEFINE tHennessy.%15
STRING 517565656E732000

! String "Intmm "
DEFINE tHennessy.%16
STRING 496E746D6D2000

! String "Mm "
DEFINE tHennessy.%17
STRING 4D6D2000

! String "Puzzle "
DEFINE tHennessy.%18
STRING 50757A7A6C652000

! String "Quick "
DEFINE tHennessy.%19
STRING 517569636B2000

! String "Bubble "
DEFINE tHennessy.%20
STRING 427562626C652000

! String "Tree "
DEFINE tHennessy.%21
STRING 547265652000

! String "FFT "
DEFINE tHennessy.%22
STRING 4646542000

! String "Done$"
DEFINE tHennessy.%23
STRING 446F6E652400

! Descriptor for nodeDesc
DEFINE tHennessy.nodeDesc
WORD 0x00000007
WORD 0
WORD tHennessy.nodeDesc.%anc

DEFINE tHennessy.nodeDesc.%anc
WORD tHennessy.nodeDesc

! Descriptor for element
DEFINE tHennessy.element
WORD 0
WORD 0
WORD tHennessy.element.%anc

DEFINE tHennessy.element.%anc
WORD tHennessy.element

! Descriptor for complex
DEFINE tHennessy.complex
WORD 0
WORD 0
WORD tHennessy.complex.%anc

DEFINE tHennessy.complex.%anc
WORD tHennessy.complex

! End of file
]]*)
