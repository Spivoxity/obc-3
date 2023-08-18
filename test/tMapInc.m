MODULE tMapInc;

(* Very simple closure-based functional programming on Keiko *)

IMPORT Out;

TYPE Value = POINTER TO Object;
  Object = RECORD END;

TYPE IntCell = RECORD(Object) value: INTEGER END;
  Integer = POINTER TO IntCell;

PROCEDURE integer(x: INTEGER): Value;
  VAR p: Integer;
BEGIN
  NEW(p);
  p.value := x;
  RETURN p
END integer;

PROCEDURE intval(x: Value): INTEGER;
  VAR p: Integer;
BEGIN
  p := x(Integer);
  RETURN p.value
END intval;

TYPE ConsCell = RECORD(Object) head, tail: Value END;
  Cons = POINTER TO ConsCell;

PROCEDURE cons(h, t: Value): Value;
  VAR p: Cons;
BEGIN
  NEW(p);
  p.head := h; p.tail := t;
  RETURN p
END cons;

PROCEDURE head(x: Value): Value;
  VAR p: Cons;
BEGIN
  p := x(Cons);
  RETURN p.head
END head;

PROCEDURE tail(x: Value): Value;
  VAR p: Cons;
BEGIN
  p := x(Cons);
  RETURN p.tail
END tail;

TYPE Frame = POINTER TO ARRAY OF Value;

TYPE Body1 = PROCEDURE (frame: Frame; x: Value): Value;

TYPE ClosCell1 = RECORD(Object)
      code: Body1;
      frame: Frame
    END;
  Closure1 = POINTER TO ClosCell1;

PROCEDURE closure1(code: Body1; frame: Frame): Value;
  VAR p: Closure1;
BEGIN
  NEW(p);
  p.code := code; p.frame := frame;
  RETURN p
END closure1;

TYPE Body2 = PROCEDURE (frame: Frame; x, y: Value): Value;

TYPE ClosCell2 = RECORD(Object)
      code: Body2;
      frame: Frame
    END;
  Closure2 = POINTER TO ClosCell2;

PROCEDURE closure2(code: Body2; frame: Frame): Value;
  VAR p: Closure2;
BEGIN
  NEW(p);
  p.code := code; p.frame := frame;
  RETURN p
END closure2;

PROCEDURE apply1(fun, x: Value): Value;
  VAR p: Closure1;
BEGIN
  p := fun(Closure1);
  RETURN p.code(p.frame, x)
END apply1;

PROCEDURE apply2(fun, x, y: Value): Value;
  VAR p: Closure2;
BEGIN
  p := fun(Closure2);
  RETURN p.code(p.frame, x, y)
END apply2;

PROCEDURE frame2(x, y: Value): Frame;
  VAR f: Frame;
BEGIN
  NEW(f, 2);
  f[0] := x; f[1] := y;
  RETURN f
END frame2;

PROCEDURE PrintList(xs: Value);
  VAR ys: Value;
BEGIN
  ys := xs;
  WHILE ys # NIL DO
    Out.Char(' '); Out.Int(intval(head(ys)), 0);
    ys := tail(ys)
  END
END PrintList;


VAR map, incall, curry, plus, square, compose, onetwothree: Value;

(* map(f, xs) =
    if null xs then nil else cons(f(head(xs)), map(f, tail(xs))) *)
PROCEDURE Map(frame: Frame; f, xs: Value): Value;
BEGIN
  IF xs = NIL THEN
    RETURN NIL
  ELSE
    RETURN cons(apply1(f, head(xs)),
                apply2(map, f, tail(xs)))
  END
END Map;

(* incall(n, xs) =
    map(compose(square, curry(plus, n)), xs) *)
PROCEDURE IncAll(frame: Frame; n, xs: Value): Value;
BEGIN
  RETURN apply2(map, apply2(compose, square,
                            apply2(curry, plus, n)), xs)
END IncAll;

(* curry(f, x) = lambda (y) f(x, y) *)
PROCEDURE Lambda(frame: Frame; y: Value): Value;
BEGIN
  RETURN apply2(frame[0], frame[1], y)
END Lambda;

PROCEDURE Curry(frame: Frame; f, x: Value): Value;
BEGIN
  RETURN closure1(Lambda, frame2(f, x))
END Curry;

(* compose(g, f) = lambda (x) g(f(x)) *)
PROCEDURE GF(frame: Frame; x: Value): Value;
BEGIN
  RETURN apply1(frame[0], apply1(frame[1], x))
END GF;

PROCEDURE Compose(frame: Frame; g, f: Value): Value;
BEGIN
  RETURN closure1(GF, frame2(g, f))
END Compose;

PROCEDURE Plus(frame: Frame; x, y: Value): Value;
BEGIN
  RETURN integer(intval(x) + intval(y))
END Plus;

PROCEDURE Square(frame: Frame; u: Value): Value;
  VAR x: INTEGER;
BEGIN
  x := intval(u);
  RETURN integer(x * x)
END Square;

BEGIN
  map := closure2(Map, NIL);
  incall := closure2(IncAll, NIL);
  curry := closure2(Curry, NIL);
  plus := closure2(Plus, NIL);
  square := closure1(Square, NIL);
  compose := closure2(Compose, NIL);
  onetwothree :=
    cons(integer(1), cons(integer(2), cons(integer(3), NIL)));

  (* incall 10 (1:2:3:[]) *)
  PrintList(apply2(incall, integer(10), onetwothree));
  Out.Ln
END tMapInc.

(*<<
 121 144 169
>>*)

(*[[
!! (SYMFILE #tMapInc STAMP #tMapInc.%main 1 #tMapInc.m)
!! (CHKSUM STAMP)
!! 
MODULE tMapInc STAMP 0
IMPORT Out STAMP
ENDHDR

PROC tMapInc.integer 4 3 0x00010001
! PROCEDURE integer(x: INTEGER): Value;
!   NEW(p);
CONST 4
GLOBAL tMapInc.IntCell
GLOBAL NEW
CALLW 2
STLW -4
!   p.value := x;
LDLW 12
LDLW -4
NCHECK 17
STOREW
!   RETURN p
LDLW -4
RETURN
END

PROC tMapInc.intval 4 4 0x00110001
! PROCEDURE intval(x: Value): INTEGER;
!   p := x(Integer);
LDLW 12
DUP 0
NCHECK 24
LDNW -4
DUP 0
LDNW 4
CONST 1
JGEQ L3
POP 1
JUMP L2
LABEL L3
LDNW 8
LDNW 4
GLOBAL tMapInc.IntCell
JEQ L1
LABEL L2
ERROR E_CAST 24
LABEL L1
STLW -4
!   RETURN p.value
LDLW -4
NCHECK 25
LOADW
RETURN
END

PROC tMapInc.cons 4 3 0x00310001
! PROCEDURE cons(h, t: Value): Value;
!   NEW(p);
CONST 8
GLOBAL tMapInc.ConsCell
GLOBAL NEW
CALLW 2
STLW -4
!   p.head := h; p.tail := t;
LDLW 12
LDLW -4
NCHECK 35
STOREW
LDLW 16
LDLW -4
NCHECK 35
STNW 4
!   RETURN p
LDLW -4
RETURN
END

PROC tMapInc.head 4 4 0x00110001
! PROCEDURE head(x: Value): Value;
!   p := x(Cons);
LDLW 12
DUP 0
NCHECK 42
LDNW -4
DUP 0
LDNW 4
CONST 1
JGEQ L6
POP 1
JUMP L5
LABEL L6
LDNW 8
LDNW 4
GLOBAL tMapInc.ConsCell
JEQ L4
LABEL L5
ERROR E_CAST 42
LABEL L4
STLW -4
!   RETURN p.head
LDLW -4
NCHECK 43
LOADW
RETURN
END

PROC tMapInc.tail 4 4 0x00110001
! PROCEDURE tail(x: Value): Value;
!   p := x(Cons);
LDLW 12
DUP 0
NCHECK 49
LDNW -4
DUP 0
LDNW 4
CONST 1
JGEQ L9
POP 1
JUMP L8
LABEL L9
LDNW 8
LDNW 4
GLOBAL tMapInc.ConsCell
JEQ L7
LABEL L8
ERROR E_CAST 49
LABEL L7
STLW -4
!   RETURN p.tail
LDLW -4
NCHECK 50
LDNW 4
RETURN
END

PROC tMapInc.closure1 4 3 0x00410001
! PROCEDURE closure1(code: Body1; frame: Frame): Value;
!   NEW(p);
CONST 8
GLOBAL tMapInc.ClosCell1
GLOBAL NEW
CALLW 2
STLW -4
!   p.code := code; p.frame := frame;
LDLW 16
GCHECK 67
LDLW 12
LDLW -4
NCHECK 67
STOREW
LDLW 20
LDLW -4
NCHECK 67
STNW 4
!   RETURN p
LDLW -4
RETURN
END

PROC tMapInc.closure2 4 3 0x00410001
! PROCEDURE closure2(code: Body2; frame: Frame): Value;
!   NEW(p);
CONST 8
GLOBAL tMapInc.ClosCell2
GLOBAL NEW
CALLW 2
STLW -4
!   p.code := code; p.frame := frame;
LDLW 16
GCHECK 83
LDLW 12
LDLW -4
NCHECK 83
STOREW
LDLW 20
LDLW -4
NCHECK 83
STNW 4
!   RETURN p
LDLW -4
RETURN
END

PROC tMapInc.apply1 4 4 0x00310001
! PROCEDURE apply1(fun, x: Value): Value;
!   p := fun(Closure1);
LDLW 12
DUP 0
NCHECK 90
LDNW -4
DUP 0
LDNW 4
CONST 1
JGEQ L12
POP 1
JUMP L11
LABEL L12
LDNW 8
LDNW 4
GLOBAL tMapInc.ClosCell1
JEQ L10
LABEL L11
ERROR E_CAST 90
LABEL L10
STLW -4
!   RETURN p.code(p.frame, x)
LDLW 16
LDLW -4
NCHECK 91
LDNW 4
LDLW -4
NCHECK 91
LOADW
NCHECK 91
CALLW 2
RETURN
END

PROC tMapInc.apply2 4 4 0x00710001
! PROCEDURE apply2(fun, x, y: Value): Value;
!   p := fun(Closure2);
LDLW 12
DUP 0
NCHECK 97
LDNW -4
DUP 0
LDNW 4
CONST 1
JGEQ L15
POP 1
JUMP L14
LABEL L15
LDNW 8
LDNW 4
GLOBAL tMapInc.ClosCell2
JEQ L13
LABEL L14
ERROR E_CAST 97
LABEL L13
STLW -4
!   RETURN p.code(p.frame, x, y)
LDLW 20
LDLW 16
LDLW -4
NCHECK 98
LDNW 4
LDLW -4
NCHECK 98
LOADW
NCHECK 98
CALLW 3
RETURN
END

PROC tMapInc.frame2 4 5 0x00310001
! PROCEDURE frame2(x, y: Value): Frame;
!   NEW(f, 2);
CONST 2
CONST 1
CONST 4
CONST 0x00000003
GLOBAL NEWFLEX
CALLW 4
STLW -4
!   f[0] := x; f[1] := y;
LDLW 12
LDLW -4
NCHECK 105
CONST 0
DUP 1
LDNW -4
LDNW 4
BOUND 105
STIW
LDLW 16
LDLW -4
NCHECK 105
CONST 1
DUP 1
LDNW -4
LDNW 4
BOUND 105
STIW
!   RETURN f
LDLW -4
RETURN
END

PROC tMapInc.PrintList 4 3 0x00110001
! PROCEDURE PrintList(xs: Value);
!   ys := xs;
LDLW 12
STLW -4
LABEL L16
!   WHILE ys # NIL DO
LDLW -4
JEQZ L18
!     Out.Char(' '); Out.Int(intval(head(ys)), 0);
CONST 32
ALIGNC
GLOBAL Out.Char
CALL 1
CONST 0
LDLW -4
GLOBAL tMapInc.head
CALLW 1
GLOBAL tMapInc.intval
CALLW 1
GLOBAL Out.Int
CALL 2
!     ys := tail(ys)
LDLW -4
GLOBAL tMapInc.tail
CALLW 1
STLW -4
JUMP L16
LABEL L18
RETURN
END

PROC tMapInc.Map 0 4 0x00700001
! PROCEDURE Map(frame: Frame; f, xs: Value): Value;
!   IF xs = NIL THEN
LDLW 20
JNEQZ L21
!     RETURN NIL
CONST 0
RETURN
LABEL L21
!     RETURN cons(apply1(f, head(xs)),
LDLW 20
GLOBAL tMapInc.tail
CALLW 1
LDLW 16
LDGW tMapInc.map
GLOBAL tMapInc.apply2
CALLW 3
LDLW 20
GLOBAL tMapInc.head
STKMAP 0x00000005
CALLW 1
LDLW 16
GLOBAL tMapInc.apply1
STKMAP 0x00000009
CALLW 2
GLOBAL tMapInc.cons
CALLW 2
RETURN
END

PROC tMapInc.IncAll 0 5 0x00700001
! PROCEDURE IncAll(frame: Frame; n, xs: Value): Value;
!   RETURN apply2(map, apply2(compose, square,
LDLW 20
LDLW 16
LDGW tMapInc.plus
LDGW tMapInc.curry
GLOBAL tMapInc.apply2
STKMAP 0x00000011
CALLW 3
LDGW tMapInc.square
LDGW tMapInc.compose
GLOBAL tMapInc.apply2
STKMAP 0x00000011
CALLW 3
LDGW tMapInc.map
GLOBAL tMapInc.apply2
CALLW 3
RETURN
END

PROC tMapInc.Lambda 0 5 0x00300001
! PROCEDURE Lambda(frame: Frame; y: Value): Value;
!   RETURN apply2(frame[0], frame[1], y)
LDLW 16
LDLW 12
NCHECK 145
CONST 1
DUP 1
LDNW -4
LDNW 4
BOUND 145
LDIW
LDLW 12
NCHECK 145
CONST 0
DUP 1
LDNW -4
LDNW 4
BOUND 145
LDIW
GLOBAL tMapInc.apply2
CALLW 3
RETURN
END

PROC tMapInc.Curry 0 4 0x00700001
! PROCEDURE Curry(frame: Frame; f, x: Value): Value;
!   RETURN closure1(Lambda, frame2(f, x))
LDLW 20
LDLW 16
GLOBAL tMapInc.frame2
CALLW 2
CONST 0
GLOBAL tMapInc.Lambda
GLOBAL tMapInc.closure1
CALLW 3
RETURN
END

PROC tMapInc.GF 0 4 0x00300001
! PROCEDURE GF(frame: Frame; x: Value): Value;
!   RETURN apply1(frame[0], apply1(frame[1], x))
LDLW 16
LDLW 12
NCHECK 156
CONST 1
DUP 1
LDNW -4
LDNW 4
BOUND 156
LDIW
GLOBAL tMapInc.apply1
CALLW 2
LDLW 12
NCHECK 156
CONST 0
DUP 1
LDNW -4
LDNW 4
BOUND 156
LDIW
GLOBAL tMapInc.apply1
CALLW 2
RETURN
END

PROC tMapInc.Compose 0 4 0x00700001
! PROCEDURE Compose(frame: Frame; g, f: Value): Value;
!   RETURN closure1(GF, frame2(g, f))
LDLW 20
LDLW 16
GLOBAL tMapInc.frame2
CALLW 2
CONST 0
GLOBAL tMapInc.GF
GLOBAL tMapInc.closure1
CALLW 3
RETURN
END

PROC tMapInc.Plus 0 3 0x00700001
! PROCEDURE Plus(frame: Frame; x, y: Value): Value;
!   RETURN integer(intval(x) + intval(y))
LDLW 16
GLOBAL tMapInc.intval
CALLW 1
LDLW 20
GLOBAL tMapInc.intval
CALLW 1
PLUS
GLOBAL tMapInc.integer
CALLW 1
RETURN
END

PROC tMapInc.Square 4 2 0x00300001
! PROCEDURE Square(frame: Frame; u: Value): Value;
!   x := intval(u);
LDLW 16
GLOBAL tMapInc.intval
CALLW 1
STLW -4
!   RETURN integer(x * x)
LDLW -4
LDLW -4
TIMES
GLOBAL tMapInc.integer
CALLW 1
RETURN
END

PROC tMapInc.%main 0 4 0
!   map := closure2(Map, NIL);
CONST 0
CONST 0
GLOBAL tMapInc.Map
GLOBAL tMapInc.closure2
CALLW 3
STGW tMapInc.map
!   incall := closure2(IncAll, NIL);
CONST 0
CONST 0
GLOBAL tMapInc.IncAll
GLOBAL tMapInc.closure2
CALLW 3
STGW tMapInc.incall
!   curry := closure2(Curry, NIL);
CONST 0
CONST 0
GLOBAL tMapInc.Curry
GLOBAL tMapInc.closure2
CALLW 3
STGW tMapInc.curry
!   plus := closure2(Plus, NIL);
CONST 0
CONST 0
GLOBAL tMapInc.Plus
GLOBAL tMapInc.closure2
CALLW 3
STGW tMapInc.plus
!   square := closure1(Square, NIL);
CONST 0
CONST 0
GLOBAL tMapInc.Square
GLOBAL tMapInc.closure1
CALLW 3
STGW tMapInc.square
!   compose := closure2(Compose, NIL);
CONST 0
CONST 0
GLOBAL tMapInc.Compose
GLOBAL tMapInc.closure2
CALLW 3
STGW tMapInc.compose
!   onetwothree :=
CONST 0
CONST 3
GLOBAL tMapInc.integer
CALLW 1
GLOBAL tMapInc.cons
CALLW 2
CONST 2
GLOBAL tMapInc.integer
STKMAP 0x00000005
CALLW 1
GLOBAL tMapInc.cons
CALLW 2
CONST 1
GLOBAL tMapInc.integer
STKMAP 0x00000005
CALLW 1
GLOBAL tMapInc.cons
CALLW 2
STGW tMapInc.onetwothree
!   PrintList(apply2(incall, integer(10), onetwothree));
LDGW tMapInc.onetwothree
CONST 10
GLOBAL tMapInc.integer
STKMAP 0x00000005
CALLW 1
LDGW tMapInc.incall
GLOBAL tMapInc.apply2
CALLW 3
GLOBAL tMapInc.PrintList
CALL 1
!   Out.Ln
GLOBAL Out.Ln
CALL 0
RETURN
END

! Global variables
GLOVAR tMapInc.map 4
GLOVAR tMapInc.incall 4
GLOVAR tMapInc.curry 4
GLOVAR tMapInc.plus 4
GLOVAR tMapInc.square 4
GLOVAR tMapInc.compose 4
GLOVAR tMapInc.onetwothree 4

! Global pointer map
DEFINE tMapInc.%gcmap
WORD GC_POINTER
WORD tMapInc.map
WORD GC_POINTER
WORD tMapInc.incall
WORD GC_POINTER
WORD tMapInc.curry
WORD GC_POINTER
WORD tMapInc.plus
WORD GC_POINTER
WORD tMapInc.square
WORD GC_POINTER
WORD tMapInc.compose
WORD GC_POINTER
WORD tMapInc.onetwothree
WORD GC_END

! Descriptor for Object
DEFINE tMapInc.Object
WORD 0
WORD 0
WORD tMapInc.Object.%anc

DEFINE tMapInc.Object.%anc
WORD tMapInc.Object

! Descriptor for IntCell
DEFINE tMapInc.IntCell
WORD 0
WORD 1
WORD tMapInc.IntCell.%anc

DEFINE tMapInc.IntCell.%anc
WORD tMapInc.Object
WORD tMapInc.IntCell

! Descriptor for ConsCell
DEFINE tMapInc.ConsCell
WORD 0x00000007
WORD 1
WORD tMapInc.ConsCell.%anc

DEFINE tMapInc.ConsCell.%anc
WORD tMapInc.Object
WORD tMapInc.ConsCell

! Descriptor for ClosCell1
DEFINE tMapInc.ClosCell1
WORD 0x00000005
WORD 1
WORD tMapInc.ClosCell1.%anc

DEFINE tMapInc.ClosCell1.%anc
WORD tMapInc.Object
WORD tMapInc.ClosCell1

! Descriptor for ClosCell2
DEFINE tMapInc.ClosCell2
WORD 0x00000005
WORD 1
WORD tMapInc.ClosCell2.%anc

DEFINE tMapInc.ClosCell2.%anc
WORD tMapInc.Object
WORD tMapInc.ClosCell2

! End of file
]]*)
