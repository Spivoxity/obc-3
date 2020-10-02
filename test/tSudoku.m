MODULE tSudoku;

IMPORT Out;

(* The Boolean matrix has:

  * 81 columns Q11 .. Q99, one for each cell, since each cell must
    contain exactly one digit.

  * Nine columns Cn1 .. Cn9 for each column n of the puzzle grid, since
    each column must contain each digit exactly once.

  * Nine columns Rn1 .. Rn9 for each row of the puzzle grid.

  * Nine columns Bn1 .. Bn9 for each block of the puzzle grid.

Each row corresponds to placing a digit in a single cell, and contains
four 1's, one in each of the four groups of columns above. *)

TYPE 
  Cell = POINTER TO RECORD 
      up, down, left, right: Cell; 	(* Neighbours *)
      column: Column;			(* Top of the column *)
    END;

  Column = POINTER TO RECORD
      name: CHAR;			(* Column code: C, R, B, Q *)
      x, y: INTEGER;			(* Two digits to identify column *)
      size: INTEGER;			(* No. of intersecting rows *)
      covered: BOOLEAN;			(* Whether covered *)
      prev, next: Column;		(* Links to adjacent columns *)
      head: Cell;			(* Dummy node for this column *)
    END;

VAR 
  root: Column; 			(* Root of the entire matrix *)

(* |PrintCol| -- print the name of a column *)
PROCEDURE PrintCol(c: Column);
BEGIN
  Out.Char(c.name); Out.Int(c.x, 0); Out.Int(c.y, 0)
END PrintCol;
    
(* |PrintRow| -- print all columns in a given row *)
PROCEDURE PrintRow(p: Cell);
  VAR q: Cell; n: INTEGER; 
BEGIN
  (* Print the columns that intersect the row *)
  q := p;
  REPEAT
    Out.String(" "); PrintCol(q.column); q := q.right
  UNTIL q = p;

  (* Print position in column *)
  n := 0; q := p.column.head;
  WHILE q # p DO INC(n); q := q.down END;
  Out.String("; # "); Out.Int(n,0); Out.String(" of ");
  Out.Int(p.column.size, 0); Out.String(" choices for ");
  PrintCol(p.column); Out.Ln
END PrintRow;


(* Creating the puzzle *)

CONST sqrtN = 3; N = sqrtN * sqrtN;

VAR
  boardCell: ARRAY N OF ARRAY N OF Column;
  boardColumn: ARRAY N OF ARRAY N OF Column;
  boardRow: ARRAY N OF ARRAY N OF Column;
  boardBlock: ARRAY N OF ARRAY N OF Column;
  boardMove: ARRAY N OF ARRAY N OF ARRAY N OF Cell;

PROCEDURE ColumnLink(r: Column; VAR p: Cell);
  VAR q: Cell;
BEGIN
  NEW(q);
  IF p = NIL THEN
    q.right := q; q.left := q; p := q
  ELSE
    q.left := p.left; q.right := p;
    p.left.right := q; p.left := q
  END;
  q.up := r.head.up; q.down := r.head;
  r.head.up.down := q; r.head.up := q;
  q.column := r; INC(r.size)
END ColumnLink;

PROCEDURE MakeArray(VAR a: ARRAY OF ARRAY OF Column; 
	            name: CHAR; m, n: INTEGER);
  VAR 
    i, j: INTEGER;
    p: Column;
BEGIN
  FOR i := 0 TO m-1 DO
    FOR j := 0 TO n-1 DO
      NEW(p); p.name := name; p.x := i+1; p.y := j+1; 
      p.size := 0; p.covered := FALSE;
      NEW(p.head); p.head.down := p.head; p.head.up := p.head;
      p.prev := root.prev; p.next := root;
      root.prev.next := p; root.prev := p;
      a[i][j] := p
    END
  END
END MakeArray;

PROCEDURE MakeMove(i, j, k: INTEGER);
  VAR p: Cell;
BEGIN
  p := NIL;
  ColumnLink(boardCell[i, j], p);
  ColumnLink(boardColumn[j, k], p);
  ColumnLink(boardRow[i, k], p);
  ColumnLink(boardBlock[sqrtN * (i DIV sqrtN) + j DIV sqrtN, k], p);
  boardMove[i, j, k] := p
END MakeMove;

PROCEDURE MakePuzzle;
  VAR i, j, k: INTEGER;
BEGIN
  NEW(root);
  root.prev := root; root.next := root;

  MakeArray(boardCell, 'Q', N, N);
  MakeArray(boardColumn, 'C', N, N);
  MakeArray(boardRow, 'R', N, N);
  MakeArray(boardBlock, 'B', N, N);

  FOR i := 0 TO N-1 DO
    FOR j := 0 TO N-1 DO
      FOR k := 0 TO N-1 DO
        MakeMove(i, j, k);
      END
    END
  END
END MakePuzzle;  


(* Exact cover problem *)

VAR 
  choice: ARRAY N*N OF Cell; 	(* Current set of choices *)
	
(* |Cover| -- temporarily remove a column *)
PROCEDURE Cover(p: Column);
  VAR q, r: Cell;
BEGIN
  p.covered := TRUE;

  (* Remove p from the list of columns *)
  p.prev.next := p.next; p.next.prev := p.prev;

  (* Block each row that intersects p *)
  q := p.head.down;
  WHILE q # p.head DO
    r := q.right;
    WHILE r # q DO
      r.up.down := r.down; r.down.up := r.up;
      DEC(r.column.size); r := r.right
    END;
    q := q.down
  END
END Cover;

(* |Uncover| -- reverse the effect of |Cover| *)
PROCEDURE Uncover(p: Column);
  VAR q, r: Cell;
BEGIN
  (* Restore p to the list of columns *)
  p.prev.next := p; p.next.prev := p;

  (* Unblock each row that intersects p *)
  q := p.head.up;
  WHILE q # p.head DO
    r := q.left;
    WHILE r # q DO
      r.up.down := r; r.down.up := r;
      INC(r.column.size); r := r.left
    END;
    q := q.up
  END;

  p.covered := FALSE
END Uncover;

(* |ChooseColumn| -- select a column according to stratregy *)
PROCEDURE ChooseColumn(): Column;
  VAR c, col: Column;
BEGIN
  (* Find smallest column |col| *)
  col := root.next;
  c := col.next;
  WHILE c # root DO
    IF c.size < col.size THEN col := c END;
    c := c.next
  END;
  RETURN col
END ChooseColumn;

PROCEDURE PrintState(level: INTEGER);
  VAR 
    i, j, k: INTEGER; 
    p: Cell;
    board: ARRAY N OF ARRAY N OF CHAR;
BEGIN
  FOR i := 0 TO N-1 DO
    FOR j := 0 TO N-1 DO
      board[i, j] := '.'
    END
  END;

  FOR k := 0 TO level-1 DO
    p := choice[k];
    WHILE p.column.name # 'Q' DO p := p.right END;
    i := p.column.x - 1; j := p.column.y - 1;
    board[i, j] := CHR(p.right.column.y + ORD('0'))
  END;

  FOR i := 0 TO N-1 DO
    Out.String(board[i]); Out.Ln
  END
END PrintState;

(* |Solve| -- find an exact cover by backtracking search *)
PROCEDURE Solve(level: INTEGER);
  VAR col: Column; p, q: Cell;
BEGIN
  IF root.next = root THEN
    Out.String("Solution:"); Out.Ln;
    PrintState(level); RETURN
  END;

  col := ChooseColumn();
  IF col.size = 0 THEN RETURN END;
  Cover(col);

  (* Try each row that intersects column col *)
  p := col.head.down;
  WHILE p # col.head DO
    choice[level] := p;

    Out.Int(level, 0); Out.String(":"); PrintRow(p);

    (* Cover other columns in row |p| *)
    q := p.right;
    WHILE q # p DO Cover(q.column); q := q.right END;

    Solve(level+1);

    (* Uncover other columns in row |p| *)
    q := p.left;
    WHILE q # p DO Uncover(q.column); q := q.left END;

    p := p.down
  END;

  Uncover(col)
END Solve;

PROCEDURE ChooseRow(VAR level: INTEGER; p: Cell);
  VAR q: Cell;
BEGIN
  choice[level] := p; INC(level);
  q := p;
  REPEAT
    IF q.column.covered THEN
      Out.String("Conflict for "); PrintCol(q.column); Out.Ln
    END;
    Cover(q.column); q := q.right
  UNTIL q = p
END ChooseRow;

CONST input =
"..3....51/5.2..64../..7.5..../...63.7../2..7.8..6/..4.21.../....7.8../..81..6.9/17....5..";

PROCEDURE Input(VAR level: INTEGER);
  VAR i, j, k: INTEGER; ch: CHAR;
BEGIN
  FOR i := 0 TO N-1 DO
    FOR j := 0 TO N-1 DO
      ch := input[10*i+j];
      Out.Char(ch);
      IF ch # '.' THEN
        k := ORD(ch) - ORD('1');
	ChooseRow(level, boardMove[i, j, k])
      END
    END;
    Out.Ln
  END
END Input;

(* Main program *)
VAR level: INTEGER;

BEGIN
  MakePuzzle;
  level := 0;
  Input(level);
  Solve(level)
END tSudoku.

(*<<
..3....51
5.2..64..
..7.5....
...63.7..
2..7.8..6
..4.21...
....7.8..
..81..6.9
17....5..
28: Q85 C54 R84 B84; # 1 of 1 choices for Q85
29: Q55 C59 R59 B59; # 1 of 1 choices for Q55
30: Q15 C58 R18 B28; # 1 of 1 choices for Q15
31: Q25 C51 R21 B21; # 1 of 1 choices for Q25
32: Q64 C45 R65 B55; # 1 of 1 choices for Q64
33: Q46 C64 R44 B54; # 1 of 1 choices for Q46
34: Q81 C13 R83 B73; # 1 of 1 choices for Q81
35: Q95 C56 R96 B86; # 1 of 1 choices for Q95
36: Q93 C39 R99 B79; # 1 of 1 choices for Q93
37: C17 R67 B47 Q61; # 1 of 1 choices for C17
38: C36 R76 B76 Q73; # 1 of 1 choices for C36
39: Q71 C14 R74 B74; # 1 of 1 choices for Q71
40: C48 R98 B88 Q94; # 1 of 1 choices for C48
41: C67 R17 B27 Q16; # 1 of 1 choices for C67
42: C71 R51 B61 Q57; # 1 of 1 choices for C71
43: Q53 C35 R55 B45; # 1 of 1 choices for Q53
44: Q43 C31 R41 B41; # 1 of 1 choices for Q43
45: Q52 C23 R53 B43; # 1 of 1 choices for Q52
46: Q58 C84 R54 B64; # 1 of 1 choices for Q58
47: C21 R31 B11 Q32; # 1 of 1 choices for C21
48: C24 R14 B14 Q12; # 1 of 1 choices for C24
49: C26 R66 B46 Q62; # 1 of 1 choices for C26
50: C44 R34 B24 Q34; # 1 of 1 choices for C44
51: C81 R71 B91 Q78; # 1 of 1 choices for C81
52: C86 R36 B36 Q38; # 1 of 1 choices for C86
53: C16 R16 B16 Q11; # 1 of 1 choices for C16
54: C94 R94 B94 Q99; # 1 of 1 choices for C94
55: C95 R45 B65 Q49; # 1 of 1 choices for C95
56: C97 R27 B37 Q29; # 1 of 1 choices for C97
57: C87 R87 B97 Q88; # 1 of 1 choices for C87
58: R42 B62 Q48 C82; # 1 of 1 choices for R42
59: Q98 C83 R93 B93; # 1 of 1 choices for Q98
60: Q79 C92 R72 B92; # 1 of 1 choices for Q79
61: Q72 C25 R75 B75; # 1 of 1 choices for Q72
62: Q82 C22 R82 B72; # 1 of 1 choices for Q82
63: Q86 C65 R85 B85; # 1 of 1 choices for Q86
64: Q96 C62 R92 B82; # 1 of 1 choices for Q96
65: C42 R12 B22 Q14; # 1 of 1 choices for C42
66: Q17 C79 R19 B39; # 1 of 1 choices for Q17
67: Q28 C88 R28 B38; # 1 of 1 choices for Q28
68: Q22 C29 R29 B19; # 1 of 1 choices for Q22
69: Q24 C43 R23 B23; # 1 of 1 choices for Q24
70: Q31 C18 R38 B18; # 1 of 1 choices for Q31
71: Q36 C69 R39 B29; # 1 of 1 choices for Q36
72: Q39 C93 R33 B33; # 1 of 1 choices for Q39
73: Q37 C72 R32 B32; # 1 of 1 choices for Q37
74: Q41 C19 R49 B49; # 1 of 1 choices for Q41
75: Q42 C28 R48 B48; # 1 of 1 choices for Q42
76: Q67 C73 R63 B63; # 1 of 1 choices for Q67
77: Q68 C89 R69 B69; # 1 of 1 choices for Q68
78: Q69 C98 R68 B68; # 1 of 1 choices for Q69
79: Q74 C49 R79 B89; # 1 of 1 choices for Q74
80: Q76 C63 R73 B83; # 1 of 1 choices for Q76
Solution:
643287951
592316487
817459263
981634725
235798146
764521398
456973812
328145679
179862534
>>*)

(*[[
!! (SYMFILE #tSudoku STAMP #tSudoku.%main 1 #tSudoku.m)
!! (CHKSUM STAMP)
!! 
MODULE tSudoku STAMP 0
IMPORT Out STAMP
ENDHDR

PROC tSudoku.PrintCol 0 3 0x00100001
! PROCEDURE PrintCol(c: Column);
!   Out.Char(c.name); Out.Int(c.x, 0); Out.Int(c.y, 0)
LDLW 12
NCHECK 41
LOADC
ALIGNC
GLOBAL Out.Char
CALL 1
CONST 0
LDLW 12
NCHECK 41
LDNW 4
GLOBAL Out.Int
CALL 2
CONST 0
LDLW 12
NCHECK 41
LDNW 8
GLOBAL Out.Int
CALL 2
RETURN
END

PROC tSudoku.PrintRow 8 3 0x00110001
! PROCEDURE PrintRow(p: Cell);
!   q := p;
LDLW 12
STLW -4
LABEL L11
!     Out.String(" "); PrintCol(q.column); q := q.right
CONST 2
GLOBAL tSudoku.%9
GLOBAL Out.String
CALL 2
LDLW -4
NCHECK 51
LDNW 16
GLOBAL tSudoku.PrintCol
CALL 1
LDLW -4
NCHECK 51
LDNW 12
STLW -4
!   UNTIL q = p;
LDLW -4
LDLW 12
JNEQ L11
!   n := 0; q := p.column.head;
CONST 0
STLW -8
LDLW 12
NCHECK 55
LDNW 16
NCHECK 55
LDNW 28
STLW -4
LABEL L13
!   WHILE q # p DO INC(n); q := q.down END;
LDLW -4
LDLW 12
JEQ L15
INCL -8
LDLW -4
NCHECK 56
LDNW 4
STLW -4
JUMP L13
LABEL L15
!   Out.String("; # "); Out.Int(n,0); Out.String(" of ");
CONST 5
GLOBAL tSudoku.%1
GLOBAL Out.String
CALL 2
CONST 0
LDLW -8
GLOBAL Out.Int
CALL 2
CONST 5
GLOBAL tSudoku.%2
GLOBAL Out.String
CALL 2
!   Out.Int(p.column.size, 0); Out.String(" choices for ");
CONST 0
LDLW 12
NCHECK 58
LDNW 16
NCHECK 58
LDNW 12
GLOBAL Out.Int
CALL 2
CONST 14
GLOBAL tSudoku.%3
GLOBAL Out.String
CALL 2
!   PrintCol(p.column); Out.Ln
LDLW 12
NCHECK 59
LDNW 16
GLOBAL tSudoku.PrintCol
CALL 1
GLOBAL Out.Ln
CALL 0
RETURN
END

PROC tSudoku.ColumnLink 4 3 0x00310001
! PROCEDURE ColumnLink(r: Column; VAR p: Cell);
!   NEW(q);
CONST 20
GLOBAL tSudoku.%7
GLOBAL NEW
CALLW 2
STLW -4
!   IF p = NIL THEN
LDLW 16
LOADW
JNEQZ L18
!     q.right := q; q.left := q; p := q
LDLW -4
LDLW -4
NCHECK 79
STNW 12
LDLW -4
LDLW -4
NCHECK 79
STNW 8
LDLW -4
LDLW 16
STOREW
JUMP L16
LABEL L18
!     q.left := p.left; q.right := p;
LDLW 16
LOADW
NCHECK 81
LDNW 8
LDLW -4
NCHECK 81
STNW 8
LDLW 16
LOADW
LDLW -4
NCHECK 81
STNW 12
!     p.left.right := q; p.left := q
LDLW -4
LDLW 16
LOADW
NCHECK 82
LDNW 8
NCHECK 82
STNW 12
LDLW -4
LDLW 16
LOADW
NCHECK 82
STNW 8
LABEL L16
!   q.up := r.head.up; q.down := r.head;
LDLW 12
NCHECK 84
LDNW 28
NCHECK 84
LOADW
LDLW -4
NCHECK 84
STOREW
LDLW 12
NCHECK 84
LDNW 28
LDLW -4
NCHECK 84
STNW 4
!   r.head.up.down := q; r.head.up := q;
LDLW -4
LDLW 12
NCHECK 85
LDNW 28
NCHECK 85
LOADW
NCHECK 85
STNW 4
LDLW -4
LDLW 12
NCHECK 85
LDNW 28
NCHECK 85
STOREW
!   q.column := r; INC(r.size)
LDLW 12
LDLW -4
NCHECK 86
STNW 16
LDLW 12
NCHECK 86
DUP 0
LDNW 12
INC
SWAP
STNW 12
RETURN
END

PROC tSudoku.MakeArray 20 5 0x00104001
! PROCEDURE MakeArray(VAR a: ARRAY OF ARRAY OF Column; 
!   FOR i := 0 TO m-1 DO
LDLW 28
DEC
STLW -16
CONST 0
STLW -4
LABEL L19
LDLW -4
LDLW -16
JGT L20
!     FOR j := 0 TO n-1 DO
LDLW 32
DEC
STLW -20
CONST 0
STLW -8
LABEL L21
LDLW -8
LDLW -20
JGT L22
!       NEW(p); p.name := name; p.x := i+1; p.y := j+1; 
CONST 32
GLOBAL tSudoku.%8
GLOBAL NEW
CALLW 2
STLW -12
LDLC 24
LDLW -12
NCHECK 97
STOREC
LDLW -4
INC
LDLW -12
NCHECK 97
STNW 4
LDLW -8
INC
LDLW -12
NCHECK 97
STNW 8
!       p.size := 0; p.covered := FALSE;
CONST 0
LDLW -12
NCHECK 98
STNW 12
CONST 0
LDLW -12
NCHECK 98
CONST 16
STIC
!       NEW(p.head); p.head.down := p.head; p.head.up := p.head;
CONST 20
GLOBAL tSudoku.%7
GLOBAL NEW
CALLW 2
LDLW -12
NCHECK 99
STNW 28
LDLW -12
NCHECK 99
LDNW 28
LDLW -12
NCHECK 99
LDNW 28
NCHECK 99
STNW 4
LDLW -12
NCHECK 99
LDNW 28
LDLW -12
NCHECK 99
LDNW 28
NCHECK 99
STOREW
!       p.prev := root.prev; p.next := root;
LDGW tSudoku.root
NCHECK 100
LDNW 20
LDLW -12
NCHECK 100
STNW 20
LDGW tSudoku.root
LDLW -12
NCHECK 100
STNW 24
!       root.prev.next := p; root.prev := p;
LDLW -12
LDGW tSudoku.root
NCHECK 101
LDNW 20
NCHECK 101
STNW 24
LDLW -12
LDGW tSudoku.root
NCHECK 101
STNW 20
!       a[i][j] := p
LDLW -12
LDLW 12
LDLW -4
LDLW 16
BOUND 102
LDLW 20
TIMES
LDLW -8
LDLW 20
BOUND 102
PLUS
STIW
!     FOR j := 0 TO n-1 DO
INCL -8
JUMP L21
LABEL L22
!   FOR i := 0 TO m-1 DO
INCL -4
JUMP L19
LABEL L20
RETURN
END

PROC tSudoku.MakeMove 4 5 0x00010001
! PROCEDURE MakeMove(i, j, k: INTEGER);
!   p := NIL;
CONST 0
STLW -4
!   ColumnLink(boardCell[i, j], p);
LOCAL -4
GLOBAL tSudoku.boardCell
LDLW 12
CONST 9
BOUND 111
CONST 9
TIMES
LDLW 16
CONST 9
BOUND 111
PLUS
LDIW
GLOBAL tSudoku.ColumnLink
CALL 2
!   ColumnLink(boardColumn[j, k], p);
LOCAL -4
GLOBAL tSudoku.boardColumn
LDLW 16
CONST 9
BOUND 112
CONST 9
TIMES
LDLW 20
CONST 9
BOUND 112
PLUS
LDIW
GLOBAL tSudoku.ColumnLink
CALL 2
!   ColumnLink(boardRow[i, k], p);
LOCAL -4
GLOBAL tSudoku.boardRow
LDLW 12
CONST 9
BOUND 113
CONST 9
TIMES
LDLW 20
CONST 9
BOUND 113
PLUS
LDIW
GLOBAL tSudoku.ColumnLink
CALL 2
!   ColumnLink(boardBlock[sqrtN * (i DIV sqrtN) + j DIV sqrtN, k], p);
LOCAL -4
GLOBAL tSudoku.boardBlock
LDLW 12
CONST 3
DIV
CONST 3
TIMES
LDLW 16
CONST 3
DIV
PLUS
CONST 9
BOUND 114
CONST 9
TIMES
LDLW 20
CONST 9
BOUND 114
PLUS
LDIW
GLOBAL tSudoku.ColumnLink
CALL 2
!   boardMove[i, j, k] := p
LDLW -4
GLOBAL tSudoku.boardMove
LDLW 12
CONST 9
BOUND 115
CONST 9
TIMES
LDLW 16
CONST 9
BOUND 115
PLUS
CONST 9
TIMES
LDLW 20
CONST 9
BOUND 115
PLUS
STIW
RETURN
END

PROC tSudoku.MakePuzzle 12 7 0
! PROCEDURE MakePuzzle;
!   NEW(root);
CONST 32
GLOBAL tSudoku.%8
GLOBAL NEW
CALLW 2
STGW tSudoku.root
!   root.prev := root; root.next := root;
LDGW tSudoku.root
LDGW tSudoku.root
NCHECK 122
STNW 20
LDGW tSudoku.root
LDGW tSudoku.root
NCHECK 122
STNW 24
!   MakeArray(boardCell, 'Q', N, N);
CONST 9
CONST 9
CONST 81
ALIGNC
CONST 9
CONST 9
GLOBAL tSudoku.boardCell
GLOBAL tSudoku.MakeArray
CALL 6
!   MakeArray(boardColumn, 'C', N, N);
CONST 9
CONST 9
CONST 67
ALIGNC
CONST 9
CONST 9
GLOBAL tSudoku.boardColumn
GLOBAL tSudoku.MakeArray
CALL 6
!   MakeArray(boardRow, 'R', N, N);
CONST 9
CONST 9
CONST 82
ALIGNC
CONST 9
CONST 9
GLOBAL tSudoku.boardRow
GLOBAL tSudoku.MakeArray
CALL 6
!   MakeArray(boardBlock, 'B', N, N);
CONST 9
CONST 9
CONST 66
ALIGNC
CONST 9
CONST 9
GLOBAL tSudoku.boardBlock
GLOBAL tSudoku.MakeArray
CALL 6
!   FOR i := 0 TO N-1 DO
CONST 0
STLW -4
LABEL L23
LDLW -4
CONST 8
JGT L24
!     FOR j := 0 TO N-1 DO
CONST 0
STLW -8
LABEL L25
LDLW -8
CONST 8
JGT L26
!       FOR k := 0 TO N-1 DO
CONST 0
STLW -12
LABEL L27
LDLW -12
CONST 8
JGT L28
!         MakeMove(i, j, k);
LDLW -12
LDLW -8
LDLW -4
GLOBAL tSudoku.MakeMove
CALL 3
!       FOR k := 0 TO N-1 DO
INCL -12
JUMP L27
LABEL L28
!     FOR j := 0 TO N-1 DO
INCL -8
JUMP L25
LABEL L26
!   FOR i := 0 TO N-1 DO
INCL -4
JUMP L23
LABEL L24
RETURN
END

PROC tSudoku.Cover 8 3 0x00118001
! PROCEDURE Cover(p: Column);
!   p.covered := TRUE;
CONST 1
LDLW 12
NCHECK 148
CONST 16
STIC
!   p.prev.next := p.next; p.next.prev := p.prev;
LDLW 12
NCHECK 151
LDNW 24
LDLW 12
NCHECK 151
LDNW 20
NCHECK 151
STNW 24
LDLW 12
NCHECK 151
LDNW 20
LDLW 12
NCHECK 151
LDNW 24
NCHECK 151
STNW 20
!   q := p.head.down;
LDLW 12
NCHECK 154
LDNW 28
NCHECK 154
LDNW 4
STLW -4
LABEL L29
!   WHILE q # p.head DO
LDLW -4
LDLW 12
NCHECK 155
LDNW 28
JEQ L31
!     r := q.right;
LDLW -4
NCHECK 156
LDNW 12
STLW -8
LABEL L32
!     WHILE r # q DO
LDLW -8
LDLW -4
JEQ L34
!       r.up.down := r.down; r.down.up := r.up;
LDLW -8
NCHECK 158
LDNW 4
LDLW -8
NCHECK 158
LOADW
NCHECK 158
STNW 4
LDLW -8
NCHECK 158
LOADW
LDLW -8
NCHECK 158
LDNW 4
NCHECK 158
STOREW
!       DEC(r.column.size); r := r.right
LDLW -8
NCHECK 159
LDNW 16
NCHECK 159
DUP 0
LDNW 12
DEC
SWAP
STNW 12
LDLW -8
NCHECK 159
LDNW 12
STLW -8
JUMP L32
LABEL L34
!     q := q.down
LDLW -4
NCHECK 161
LDNW 4
STLW -4
JUMP L29
LABEL L31
RETURN
END

PROC tSudoku.Uncover 8 3 0x00118001
! PROCEDURE Uncover(p: Column);
!   p.prev.next := p; p.next.prev := p;
LDLW 12
LDLW 12
NCHECK 170
LDNW 20
NCHECK 170
STNW 24
LDLW 12
LDLW 12
NCHECK 170
LDNW 24
NCHECK 170
STNW 20
!   q := p.head.up;
LDLW 12
NCHECK 173
LDNW 28
NCHECK 173
LOADW
STLW -4
LABEL L35
!   WHILE q # p.head DO
LDLW -4
LDLW 12
NCHECK 174
LDNW 28
JEQ L37
!     r := q.left;
LDLW -4
NCHECK 175
LDNW 8
STLW -8
LABEL L38
!     WHILE r # q DO
LDLW -8
LDLW -4
JEQ L40
!       r.up.down := r; r.down.up := r;
LDLW -8
LDLW -8
NCHECK 177
LOADW
NCHECK 177
STNW 4
LDLW -8
LDLW -8
NCHECK 177
LDNW 4
NCHECK 177
STOREW
!       INC(r.column.size); r := r.left
LDLW -8
NCHECK 178
LDNW 16
NCHECK 178
DUP 0
LDNW 12
INC
SWAP
STNW 12
LDLW -8
NCHECK 178
LDNW 8
STLW -8
JUMP L38
LABEL L40
!     q := q.up
LDLW -4
NCHECK 180
LOADW
STLW -4
JUMP L35
LABEL L37
!   p.covered := FALSE
CONST 0
LDLW 12
NCHECK 183
CONST 16
STIC
RETURN
END

PROC tSudoku.ChooseColumn 8 3 0x00018001
! PROCEDURE ChooseColumn(): Column;
!   col := root.next;
LDGW tSudoku.root
NCHECK 191
LDNW 24
STLW -8
!   c := col.next;
LDLW -8
NCHECK 192
LDNW 24
STLW -4
LABEL L41
!   WHILE c # root DO
LDLW -4
LDGW tSudoku.root
JEQ L43
!     IF c.size < col.size THEN col := c END;
LDLW -4
NCHECK 194
LDNW 12
LDLW -8
NCHECK 194
LDNW 12
JGEQ L46
LDLW -4
STLW -8
LABEL L46
!     c := c.next
LDLW -4
NCHECK 195
LDNW 24
STLW -4
JUMP L41
LABEL L43
!   RETURN col
LDLW -8
RETURN
END

PROC tSudoku.PrintState 104 5 0x00002001
! PROCEDURE PrintState(level: INTEGER);
!   FOR i := 0 TO N-1 DO
CONST 0
STLW -4
LABEL L47
LDLW -4
CONST 8
JGT L48
!     FOR j := 0 TO N-1 DO
CONST 0
STLW -8
LABEL L49
LDLW -8
CONST 8
JGT L50
!       board[i, j] := '.'
CONST 46
LOCAL -97
LDLW -4
CONST 9
BOUND 208
CONST 9
TIMES
LDLW -8
CONST 9
BOUND 208
PLUS
STIC
!     FOR j := 0 TO N-1 DO
INCL -8
JUMP L49
LABEL L50
!   FOR i := 0 TO N-1 DO
INCL -4
JUMP L47
LABEL L48
!   FOR k := 0 TO level-1 DO
LDLW 12
DEC
STLW -104
CONST 0
STLW -12
LABEL L51
LDLW -12
LDLW -104
JGT L52
!     p := choice[k];
GLOBAL tSudoku.choice
LDLW -12
CONST 81
BOUND 213
LDIW
STLW -16
LABEL L53
!     WHILE p.column.name # 'Q' DO p := p.right END;
LDLW -16
NCHECK 214
LDNW 16
NCHECK 214
LOADC
CONST 81
JEQ L55
LDLW -16
NCHECK 214
LDNW 12
STLW -16
JUMP L53
LABEL L55
!     i := p.column.x - 1; j := p.column.y - 1;
LDLW -16
NCHECK 215
LDNW 16
NCHECK 215
LDNW 4
DEC
STLW -4
LDLW -16
NCHECK 215
LDNW 16
NCHECK 215
LDNW 8
DEC
STLW -8
!     board[i, j] := CHR(p.right.column.y + ORD('0'))
LDLW -16
NCHECK 216
LDNW 12
NCHECK 216
LDNW 16
NCHECK 216
LDNW 8
CONST 48
PLUS
CONVNC
LOCAL -97
LDLW -4
CONST 9
BOUND 216
CONST 9
TIMES
LDLW -8
CONST 9
BOUND 216
PLUS
STIC
!   FOR k := 0 TO level-1 DO
INCL -12
JUMP L51
LABEL L52
!   FOR i := 0 TO N-1 DO
CONST 0
STLW -4
LABEL L56
LDLW -4
CONST 8
JGT L57
!     Out.String(board[i]); Out.Ln
CONST 9
LOCAL -97
LDLW -4
CONST 9
BOUND 220
CONST 9
TIMES
OFFSET
GLOBAL Out.String
CALL 2
GLOBAL Out.Ln
CALL 0
!   FOR i := 0 TO N-1 DO
INCL -4
JUMP L56
LABEL L57
RETURN
END

PROC tSudoku.Solve 12 4 0x0001c001
! PROCEDURE Solve(level: INTEGER);
!   IF root.next = root THEN
LDGW tSudoku.root
NCHECK 228
LDNW 24
LDGW tSudoku.root
JNEQ L60
!     Out.String("Solution:"); Out.Ln;
CONST 10
GLOBAL tSudoku.%4
GLOBAL Out.String
CALL 2
GLOBAL Out.Ln
CALL 0
!     PrintState(level); RETURN
LDLW 12
GLOBAL tSudoku.PrintState
CALL 1
RETURN
LABEL L60
!   col := ChooseColumn();
GLOBAL tSudoku.ChooseColumn
CALLW 0
STLW -4
!   IF col.size = 0 THEN RETURN END;
LDLW -4
NCHECK 234
LDNW 12
JNEQZ L63
RETURN
LABEL L63
!   Cover(col);
LDLW -4
GLOBAL tSudoku.Cover
CALL 1
!   p := col.head.down;
LDLW -4
NCHECK 238
LDNW 28
NCHECK 238
LDNW 4
STLW -8
LABEL L64
!   WHILE p # col.head DO
LDLW -8
LDLW -4
NCHECK 239
LDNW 28
JEQ L66
!     choice[level] := p;
LDLW -8
GLOBAL tSudoku.choice
LDLW 12
CONST 81
BOUND 240
STIW
!     Out.Int(level, 0); Out.String(":"); PrintRow(p);
CONST 0
LDLW 12
GLOBAL Out.Int
CALL 2
CONST 2
GLOBAL tSudoku.%10
GLOBAL Out.String
CALL 2
LDLW -8
GLOBAL tSudoku.PrintRow
CALL 1
!     q := p.right;
LDLW -8
NCHECK 245
LDNW 12
STLW -12
LABEL L67
!     WHILE q # p DO Cover(q.column); q := q.right END;
LDLW -12
LDLW -8
JEQ L69
LDLW -12
NCHECK 246
LDNW 16
GLOBAL tSudoku.Cover
CALL 1
LDLW -12
NCHECK 246
LDNW 12
STLW -12
JUMP L67
LABEL L69
!     Solve(level+1);
LDLW 12
INC
GLOBAL tSudoku.Solve
CALL 1
!     q := p.left;
LDLW -8
NCHECK 251
LDNW 8
STLW -12
LABEL L70
!     WHILE q # p DO Uncover(q.column); q := q.left END;
LDLW -12
LDLW -8
JEQ L72
LDLW -12
NCHECK 252
LDNW 16
GLOBAL tSudoku.Uncover
CALL 1
LDLW -12
NCHECK 252
LDNW 8
STLW -12
JUMP L70
LABEL L72
!     p := p.down
LDLW -8
NCHECK 254
LDNW 4
STLW -8
JUMP L64
LABEL L66
!   Uncover(col)
LDLW -4
GLOBAL tSudoku.Uncover
CALL 1
RETURN
END

PROC tSudoku.ChooseRow 4 4 0x00310001
! PROCEDURE ChooseRow(VAR level: INTEGER; p: Cell);
!   choice[level] := p; INC(level);
LDLW 16
GLOBAL tSudoku.choice
LDLW 12
LOADW
CONST 81
BOUND 263
STIW
LDLW 12
DUP 0
LOADW
INC
SWAP
STOREW
!   q := p;
LDLW 16
STLW -4
LABEL L73
!     IF q.column.covered THEN
LDLW -4
NCHECK 266
LDNW 16
NCHECK 266
CONST 16
LDIC
JEQZ L77
!       Out.String("Conflict for "); PrintCol(q.column); Out.Ln
CONST 14
GLOBAL tSudoku.%5
GLOBAL Out.String
CALL 2
LDLW -4
NCHECK 267
LDNW 16
GLOBAL tSudoku.PrintCol
CALL 1
GLOBAL Out.Ln
CALL 0
LABEL L77
!     Cover(q.column); q := q.right
LDLW -4
NCHECK 269
LDNW 16
GLOBAL tSudoku.Cover
CALL 1
LDLW -4
NCHECK 269
LDNW 12
STLW -4
!   UNTIL q = p
LDLW -4
LDLW 16
JNEQ L73
RETURN
END

PROC tSudoku.Input 16 4 0x00100001
! PROCEDURE Input(VAR level: INTEGER);
!   FOR i := 0 TO N-1 DO
CONST 0
STLW -4
LABEL L78
LDLW -4
CONST 8
JGT L79
!     FOR j := 0 TO N-1 DO
CONST 0
STLW -8
LABEL L80
LDLW -8
CONST 8
JGT L81
!       ch := input[10*i+j];
GLOBAL tSudoku.%6
LDLW -4
CONST 10
TIMES
LDLW -8
PLUS
CONST 90
BOUND 281
LDIC
STLC -13
!       Out.Char(ch);
LDLC -13
ALIGNC
GLOBAL Out.Char
CALL 1
!       IF ch # '.' THEN
LDLC -13
CONST 46
JEQ L84
!         k := ORD(ch) - ORD('1');
LDLC -13
CONST 49
MINUS
STLW -12
! 	ChooseRow(level, boardMove[i, j, k])
GLOBAL tSudoku.boardMove
LDLW -4
CONST 9
BOUND 285
CONST 9
TIMES
LDLW -8
CONST 9
BOUND 285
PLUS
CONST 9
TIMES
LDLW -12
CONST 9
BOUND 285
PLUS
LDIW
LDLW 12
GLOBAL tSudoku.ChooseRow
CALL 2
LABEL L84
!     FOR j := 0 TO N-1 DO
INCL -8
JUMP L80
LABEL L81
!     Out.Ln
GLOBAL Out.Ln
CALL 0
!   FOR i := 0 TO N-1 DO
INCL -4
JUMP L78
LABEL L79
RETURN
END

PROC tSudoku.%main 0 2 0
!   MakePuzzle;
GLOBAL tSudoku.MakePuzzle
CALL 0
!   level := 0;
CONST 0
STGW tSudoku.level
!   Input(level);
GLOBAL tSudoku.level
GLOBAL tSudoku.Input
CALL 1
!   Solve(level)
LDGW tSudoku.level
GLOBAL tSudoku.Solve
CALL 1
RETURN
END

! Global variables
GLOVAR tSudoku.root 4
GLOVAR tSudoku.boardCell 324
GLOVAR tSudoku.boardColumn 324
GLOVAR tSudoku.boardRow 324
GLOVAR tSudoku.boardBlock 324
GLOVAR tSudoku.boardMove 2916
GLOVAR tSudoku.choice 324
GLOVAR tSudoku.level 4

! Global pointer map
DEFINE tSudoku.%gcmap
WORD GC_POINTER
WORD tSudoku.root
WORD GC_BASE
WORD tSudoku.boardCell
WORD GC_BLOCK
WORD 0
WORD 81
WORD GC_BASE
WORD tSudoku.boardColumn
WORD GC_BLOCK
WORD 0
WORD 81
WORD GC_BASE
WORD tSudoku.boardRow
WORD GC_BLOCK
WORD 0
WORD 81
WORD GC_BASE
WORD tSudoku.boardBlock
WORD GC_BLOCK
WORD 0
WORD 81
WORD GC_BASE
WORD tSudoku.boardMove
WORD GC_BLOCK
WORD 0
WORD 729
WORD GC_BASE
WORD tSudoku.choice
WORD GC_BLOCK
WORD 0
WORD 81
WORD GC_END

! String "; # "
DEFINE tSudoku.%1
STRING 3B20232000

! String " of "
DEFINE tSudoku.%2
STRING 206F662000

! String " choices for "
DEFINE tSudoku.%3
STRING 2063686F6963657320666F722000

! String "Solution:"
DEFINE tSudoku.%4
STRING 536F6C7574696F6E3A00

! String "Conflict for "
DEFINE tSudoku.%5
STRING 436F6E666C69637420666F722000

! String "..3....51/5.2..64../..7.5..../...63.7../2..7.8..6/..4.21.../....7.8../..81..6.9/17....5.."
DEFINE tSudoku.%6
STRING 2E2E332E2E2E2E35312F352E322E2E36342E2E2F2E2E372E352E2E2E2E2F2E2E
STRING 2E36332E372E2E2F322E2E372E382E2E362F2E2E342E32312E2E2E2F2E2E2E2E
STRING 372E382E2E2F2E2E38312E2E362E392F31372E2E2E2E352E2E00

! String " "
DEFINE tSudoku.%9
STRING 2000

! String ":"
DEFINE tSudoku.%10
STRING 3A00

! Descriptor for *anon*
DEFINE tSudoku.%7
WORD 0x0000003f
WORD 0
WORD tSudoku.%7.%anc

DEFINE tSudoku.%7.%anc
WORD tSudoku.%7

! Descriptor for *anon*
DEFINE tSudoku.%8
WORD 0x000001c1
WORD 0
WORD tSudoku.%8.%anc

DEFINE tSudoku.%8.%anc
WORD tSudoku.%8

! End of file
]]*)
