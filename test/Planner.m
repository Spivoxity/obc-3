MODULE Planner;

IMPORT In, Out, M := MathL, Conv, SYSTEM;

TYPE R = LONGREAL;

TYPE word = ARRAY 32 OF CHAR;

(* Information about a town is stored as a structure with many links. *)

  town = POINTER TO townrec;
  road = POINTER TO roadrec;

  townrec = RECORD
    Tx, Ty: R;		(* X and Y coordinates *)
    Tdist: R;		(* Best known distance from start *)
    Troads: road;		(* List of roads starting here *)
    Thlink: town;		(* Next town in hash chain *)
    Tprev, Tnext: town;		(* Prev and Next towns in queue *)
    Tbacklink: town;		(* Prev town on shortest path *)
    Tname: word;		(* Name of the town *)
  END;

  roadrec = RECORD
    Rdest: town;		(* Town reached by road *)
    Rlength: R;		(* Length of road *)
    Rnext: road;		(* Next road from same source *)
  END;    

(* The towns live in a small hash table, so that a town can be found
   quickly, given its name. *)

CONST HSIZE = 127;

VAR hashtable: ARRAY HSIZE OF town;

(* There's a priority queue of towns, ordered by best known distance
   from the origin.  We implement it as a doubly-linked ordered list. *)

VAR qhead: town;		(* List head *)

VAR
  hflag: BOOLEAN;		(* If true, use the Euclidean heuristic *)
  goal: town;			(* Goal town (used by heuristic) *)

(* ---------------------------------------------------------------- *)

PROCEDURE Panic(msg: ARRAY OF CHAR);
BEGIN
  Out.String(msg); Out.Ln;
  HALT(2)
END Panic;

PROCEDURE Split(VAR line: ARRAY OF CHAR; VAR words: ARRAY OF word): SHORTINT;
  VAR i, j: SHORTINT; n: INTEGER;
BEGIN
  n := 0; i := 0;
  LOOP
    WHILE line[i] = ' ' DO INC(i) END;
    IF line[i] = 0X THEN EXIT END;
    IF line[i] = '{' THEN
      INC(i); j := 0;
      WHILE (line[i] # '}') & (line[i] # 0X) DO
	words[n][j] := line[i];
	INC(i); INC(j)
      END;
      words[n][j] := 0X;
      IF line[i] = '}' THEN INC(i) END
    ELSE
      j := 0;
      WHILE (line[i] # ' ') & (line[i] # 0X) DO
	words[n][j] := line[i];
	INC(i); INC(j)
      END;
      words[n][j] := 0X
    END;
    INC(n)
  END;
  RETURN SHORT(n)
END Split;

(* euclid -- Euclidean distance between towns *)
PROCEDURE Euclid(t, u: town): R;
  VAR dx, dy: R;
BEGIN
  dx := t.Tx - u.Tx; dy := t.Ty - u.Ty;
  (* Out.LongReal(dx, 0); Out.Ln;
  Out.LongReal(dy, 0); Out.Ln; *)
  RETURN M.Sqrt(dx*dx + dy*dy)
END Euclid;

(* hash -- compute hash function of string *)
PROCEDURE Hash(VAR name: word): INTEGER;
  VAR h: INTEGER; i: SHORTINT;
BEGIN
  h := 0; i := 0;
  WHILE name[i] # 0X DO
    h := (5 * h + ORD(name[i])) MOD HSIZE;
    INC(i)
  END;
  RETURN h
END Hash;

(* lookup -- find a town in the hash table *)
PROCEDURE Lookup(VAR name: word): town;
  VAR h: INTEGER; t: town;
BEGIN
  h := Hash(name);
  t := hashtable[h];
  WHILE t # NIL DO
    IF name = t.Tname THEN RETURN t END;
    t := t.Thlink
  END;
  Panic("Unknown town");
  RETURN NIL
END Lookup;

(* add_town -- add a new town to the hash table *)
PROCEDURE AddTown(VAR name: word; x, y: R);
  VAR h: INTEGER; t: town;
BEGIN
  h := Hash(name);
  NEW(t); NEW(t);
  t.Tname := name;
  t.Tx := x; t.Ty := y;
  t.Tdist := 0.0;
  t.Troads := NIL;
  t.Thlink := hashtable[h];
  t.Tprev := NIL; t.Tnext := NIL;
  t.Tbacklink := NIL;
  hashtable[h] := t
END AddTown;

(* add_road -- add a road and its converse *)
PROCEDURE AddRoad(VAR name1, name2: word; leng: R);
  VAR t1, t2: town; r1, r2: road;
BEGIN
  t1 := Lookup(name1); t2 := Lookup(name2);
  NEW(r1); NEW(r1); NEW(r2);

  IF leng < Euclid(t1, t2) THEN
    Out.LongReal(leng, 0); Out.Ln;
    Out.LongReal(Euclid(t1, t2), 0); Out.Ln;
    Panic("Road is too short")
  END;

  r1.Rdest := t2; r2.Rdest := t1;
  r1.Rlength := leng; r2.Rlength := leng;
  r1.Rnext := t1.Troads; t1.Troads := r1;
  r2.Rnext := t2.Troads; t2.Troads := r2;
END AddRoad;

(* ---------------------------------------------------------------- *)

(* There's a priority queue of towns, ordered by best known distance
   from the origin.  We implement it as a doubly-linked ordered list. *)

(* init_queue -- initialize the queue *)
PROCEDURE InitQueue();
BEGIN
  NEW(qhead);
  qhead.Tdist := -1.0;
  qhead.Tnext := qhead;
  qhead.Tprev := qhead
END InitQueue;

(* clear_queue -- set the queue to be empty *)
PROCEDURE ClearQueue();
BEGIN
  qhead.Tnext := qhead;
  qhead.Tprev := qhead
END ClearQueue;

(* enqueue -- add a town to the queue *)
PROCEDURE Enqueue(t: town);
  VAR p, q: town;
BEGIN
     (* Since towns are visited in order of increasing distance, it
        seems reasonable to BEGIN the insertion PROCEDUREess from the tail
        of the queue *)

     p := qhead.Tprev;
     WHILE t.Tdist < p.Tdist DO p := p.Tprev END;
     q := p.Tnext;
     t.Tprev := p; t.Tnext := q;
     p.Tnext := t; q.Tprev := t;
END Enqueue;

(* delmin -- delete and return the first town in the queue *)
PROCEDURE Delmin(): town;
  VAR p, q: town;
BEGIN
     p := qhead.Tnext;
     IF p = qhead THEN RETURN NIL END;
     q := p.Tnext;
     qhead.Tnext := q; q.Tprev := qhead; 
     RETURN p;
END Delmin;

(* requeue -- a town has reduced in distance: adjust its place in the queue *)
PROCEDURE Requeue(t: town);
  VAR p, q: town;
BEGIN
     p := t.Tprev; q := t.Tnext;
     p.Tnext := q; q.Tprev := p;
     WHILE t.Tdist < p.Tdist DO p := p.Tprev END;
     q := p.Tnext;
     t.Tprev := p; t.Tnext := q;
     p.Tnext := t; q.Tprev := t
END Requeue;

(* ---------------------------------------------------------------- *)

(* Now here's our implementation of Dijkstra's algorithm *)

(* init -- initialize the search *)
PROCEDURE Init(src, dst: town; heur: BOOLEAN);
  VAR i: INTEGER; t: town;
BEGIN
  ClearQueue();

  FOR i := 0 TO HSIZE-1 DO
    t := hashtable[i];
    WHILE t # NIL DO
      t.Tbacklink := NIL;
      t := t.Thlink
    END
  END;         

  src.Tdist := 0.0;
  src.Tbacklink := src;
  goal := dst;
  hflag := heur;
END Init;

(* show_link -- send command to colour a road *)
PROCEDURE ShowLink(t: town; colour: ARRAY OF CHAR);
BEGIN
  Out.String("colour {"); Out.String(t.Tname); Out.String("} {");
  Out.String(t.Tbacklink.Tname); Out.String("} ");
  Out.String(colour); Out.Ln
END ShowLink;

(* visit_neighbours -- update distances to neighbours of a town *)
PROCEDURE VisitNeighbours(t: town);
  VAR r: road; u: town; d: R;
BEGIN
  r := t.Troads;
  WHILE r # NIL DO
    u := r.Rdest;
    d := t.Tdist + r.Rlength;

    (* Heuristic: reduce the length of each road by the distance
       it takes us closer to the goal. *)
    IF hflag THEN d := d - Euclid(t, goal) + Euclid(u, goal) END;

    IF u.Tbacklink = NIL THEN
      (* First time we've seen u *)
      u.Tdist := d;
      u.Tbacklink := t;
      ShowLink(u, "green");
      Enqueue(u);
    ELSIF d < u.Tdist THEN
      (* Previous link to u has been improved *)
      ShowLink(u, "brown");
      u.Tdist := d;
      u.Tbacklink := t;
      ShowLink(u, "green");
      Requeue(u)
    END;

    r := r.Rnext
  END
END VisitNeighbours;

(* search -- main search function *)
PROCEDURE Search(VAR sname, dname: word; heur: BOOLEAN);
  VAR t, src, dst: town; d: R;
BEGIN
  src := Lookup(sname);
  dst := Lookup(dname);

  Init(src, dst, heur);
  t := src;

  SYSTEM.GC;

  WHILE t # dst DO
    IF t # src THEN
      Out.String("paint {"); Out.String(t.Tname); 
      Out.String("} white"); Out.Ln
    END;
    VisitNeighbours(t);
    Out.String("pause"); Out.Ln;

    t := Delmin();
    IF t = NIL THEN
      Out.String("unreachable"); Out.Ln;
      RETURN
    END;

    ShowLink(t, "blue");
  END;

  Out.String("pause"); Out.Ln;

  (* Show the shortest route in yellow *)
  t := dst;
  WHILE t # src DO
    ShowLink(t, "yellow");
    t := t.Tbacklink
  END;

  d := dst.Tdist;
  IF hflag THEN d := d + Euclid(src, dst) END;
  Out.String("found "); Out.Fixed(d, 0, 1); Out.Ln;
END Search;

(* main -- main program *)
PROCEDURE Main();
  VAR nwords: INTEGER;
    buf: ARRAY 256 OF CHAR;
    words: ARRAY 10 OF word;
BEGIN
  InitQueue();

  LOOP
    In.Line(buf);
    IF ~ In.Done THEN EXIT END;
    (* Out.String(buf); Out.Ln; *)
    nwords := Split(buf, words);

    IF (nwords = 4) & (words[0] = "town") THEN
      AddTown(words[1], Conv.RealVal(words[2]), Conv.RealVal(words[3]))
    ELSIF (nwords = 4) & (words[0] = "road") THEN
      AddRoad(words[1], words[2], Conv.RealVal(words[3]))
    ELSIF (nwords = 4) & (words[0] = "search") THEN
      Search(words[1], words[2], (words[3][0] = '1'))
    ELSE
      Panic("bad command")
    END
  END
END Main;

BEGIN
  Main()
END Planner.
