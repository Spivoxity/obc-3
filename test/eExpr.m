MODULE eExpr;

IMPORT SYSTEM;

TYPE ar = ABSTRACT RECORD END;

VAR aa: ARRAY -10 OF ar; bb: POINTER TO ARRAY OF ar;
  cc: ARRAY OF CHAR;

PROCEDURE one(CONST x*: POINTER TO ar): xxx;
  VAR p*: POINTER TO ar;
    q: POINTER TO ARRAY 10 OF INTEGER;
BEGIN
  NEW;
  NEW(p);
  NEW(q, 2);
  NEW(x)
END one;

PROCEDURE two;
  VAR a: POINTER TO ARRAY OF INTEGER;
BEGIN
  LEN; LEN(1, 2, 3); LEN(1);
  LEN(a^, -1)
END two;

PROCEDURE three;
  VAR a: ARRAY 2 OF INTEGER;
BEGIN
  SYSTEM.VAL(ar, 4);
  SYSTEM.VAL(INTEGER, a);
  SYSTEM.VAL(3, 4)
END three;

PROCEDURE four;
  VAR p: PROCEDURE (): INTEGER;
BEGIN
  p := four
END four;

PROCEDURE five(p: PROCEDURE);
  VAR a: ARRAY 5 OF PROCEDURE;
BEGIN
  five(a[3])
END five;

PROCEDURE six(VAR a: ARRAY 10 OF INTEGER);
  VAR b: ARRAY 10 OF INTEGER; x: INTEGER;
BEGIN
  six(b); six(x)
END six;

PROCEDURE seven;
  VAR x, y: INTEGER; a: ARRAY 3 OF INTEGER;
BEGIN
  y := x(x);
  y := x(a[0]);
  y := x(z);
  y := z(INTEGER)
END seven;

BEGIN
  INC;
  INCL(3, 2);
  ASSERT;
  SYSTEM.ADR(3);
  ABS('x'); ABS(1); ABS(-1); ABS(1.0); ABS(-1.0);
  MIN(ar);
  SHORT('x'); LONG('x');
  LONG(SHORT(3));
  ENTIER(3);
  ODD(3);
  ORD(1.0);
  LSL();
  six;
  x := 3 IN 1;
  y := 'x' = 'xx';
  MIN(BOOLEAN); MIN(CHAR); MIN(SET); MIN(LONGINT);
  MIN(3); MIN(w);
  SIZE(e); SIZE(3);
  SYSTEM.VAL(r, 3);
  x := x IS k
END eExpr.

(*<<
"eExpr.m", line 7: arrays cannot have abstract record types as elements
> VAR aa: ARRAY -10 OF ar; bb: POINTER TO ARRAY OF ar;
>                      ^^

"eExpr.m", line 7: upper bound of array must be >= 0
> VAR aa: ARRAY -10 OF ar; bb: POINTER TO ARRAY OF ar;
>               ^^^

"eExpr.m", line 7: open arrays cannot have abstract record types as elements
> VAR aa: ARRAY -10 OF ar; bb: POINTER TO ARRAY OF ar;
>                                                  ^^

"eExpr.m", line 8: an open array type is not allowed here
>   cc: ARRAY OF CHAR;
>       ^^^^^^^^^^^^^
> This expression has type ARRAY OF CHAR

"eExpr.m", line 10: cannot export a parameter
> PROCEDURE one(CONST x*: POINTER TO ar): xxx;
>                     ^

"eExpr.m", line 10: 'xxx' has not been declared
> PROCEDURE one(CONST x*: POINTER TO ar): xxx;
>                                         ^^^

"eExpr.m", line 47: warning -- you should name this parameter type, or else
no actual parameter will match
> PROCEDURE six(VAR a: ARRAY 10 OF INTEGER);
>                      ^^^^^^^^^^^^^^^^^^^

"eExpr.m", line 11: cannot export a local variable
>   VAR p*: POINTER TO ar;
>       ^

"eExpr.m", line 14: NEW expects 1 or 2 arguments
>   NEW;
>   ^^^

"eExpr.m", line 15: cannot create instance of abstract record type
>   NEW(p);
>   ^^^

"eExpr.m", line 16: NEW expects 1 argument
>   NEW(q, 2);
>   ^^^

"eExpr.m", line 17: the argument of NEW must be a variable
>   NEW(x)
>       ^

"eExpr.m", line 17: cannot create instance of abstract record type
>   NEW(x)
>   ^^^

"eExpr.m", line 23: LEN requires 1 or 2 arguments
>   LEN; LEN(1, 2, 3); LEN(1);
>   ^^^

"eExpr.m", line 23: LEN requires 1 or 2 arguments
>   LEN; LEN(1, 2, 3); LEN(1);
>        ^^^^^^^^^^^^

"eExpr.m", line 23: the argument of LEN must be an array
>   LEN; LEN(1, 2, 3); LEN(1);
>                          ^
> This expression has type integer

"eExpr.m", line 24: this argument of LEN should be a non-negative integer
>   LEN(a^, -1)
>           ^^

"eExpr.m", line 30: scalar type expected in SYSTEM.VAL
>   SYSTEM.VAL(ar, 4);
>              ^^

"eExpr.m", line 31: scalar value expected in SYSTEM.VAL
>   SYSTEM.VAL(INTEGER, a);
>                       ^

"eExpr.m", line 31: argument size must match result type in SYSTEM.VAL
>   SYSTEM.VAL(INTEGER, a);
>                       ^

"eExpr.m", line 32: the first argument of SYSTEM.VAL must be a type name
>   SYSTEM.VAL(3, 4)
>              ^

"eExpr.m", line 38: type PROCEDURE ... is needed on the RHS of this
assignment
>   p := four
>        ^^^^
> This expression has type PROCEDURE ...

"eExpr.m", line 50: VAR parameter 'a' does not have the same type
>   six(b); six(x)
>       ^
> This expression has type ARRAY 10 OF INTEGER

"eExpr.m", line 50: VAR parameter 'a' should have type ARRAY 10 OF INTEGER
>   six(b); six(x)
>               ^
> This expression has type INTEGER

"eExpr.m", line 56: a procedure is needed here
>   y := x(x);
>        ^
> This expression has type INTEGER

"eExpr.m", line 57: a procedure is needed here
>   y := x(a[0]);
>        ^
> This expression has type INTEGER

"eExpr.m", line 58: 'z' has not been declared
>   y := x(z);
>          ^

"eExpr.m", line 63: INC expects 1 or 2 arguments
>   INC;
>   ^^^

"eExpr.m", line 64: type SET is needed as an argument of INCL
>   INCL(3, 2);
>        ^
> This expression has type integer

"eExpr.m", line 64: the argument of INCL must be a variable
>   INCL(3, 2);
>        ^

"eExpr.m", line 65: ASSERT expects 1 or 2 arguments
>   ASSERT;
>   ^^^^^^

"eExpr.m", line 66: the argument of SYSTEM.ADR must be a variable
>   SYSTEM.ADR(3);
>              ^

"eExpr.m", line 67: ABS needs a numeric argument
>   ABS('x'); ABS(1); ABS(-1); ABS(1.0); ABS(-1.0);
>       ^^^
> This expression has type CHAR

"eExpr.m", line 68: the argument of MIN must be a basic or enumerated type
>   MIN(ar);
>       ^^

"eExpr.m", line 69: the argument of SHORT must have type INTEGER, LONGINT or
LONGREAL
>   SHORT('x'); LONG('x');
>         ^^^
> This expression has type CHAR

"eExpr.m", line 69: the argument of LONG must have type SHORTINT, INTEGER or
REAL
>   SHORT('x'); LONG('x');
>                    ^^^
> This expression has type CHAR

"eExpr.m", line 71: the argument of ENTIER must be have a real type
>   ENTIER(3);
>          ^
> This expression has type integer

"eExpr.m", line 73: the argument of ORD must have a discrete type
>   ORD(1.0);
>       ^^^
> This expression has type REAL

"eExpr.m", line 74: LSL expects 2 arguments
>   LSL();
>   ^^^

"eExpr.m", line 75: this procedure expects 1 argument
>   six;
>   ^^^

"eExpr.m", line 76: 'x' has not been declared
>   x := 3 IN 1;
>   ^

"eExpr.m", line 76: the operands of IN must be an integer and a set
>   x := 3 IN 1;
>        ^^^^^^
> The left operand has type integer
> The right operand has type integer

"eExpr.m", line 77: 'y' has not been declared
>   y := 'x' = 'xx';
>   ^

"eExpr.m", line 79: the argument of MIN must be a basic or enumerated type
>   MIN(3); MIN(w);
>       ^

"eExpr.m", line 79: 'w' has not been declared
>   MIN(3); MIN(w);
>               ^

"eExpr.m", line 80: 'e' has not been declared
>   SIZE(e); SIZE(3);
>        ^

"eExpr.m", line 80: the argument of SIZE must be a type name
>   SIZE(e); SIZE(3);
>                 ^

"eExpr.m", line 81: 'r' has not been declared
>   SYSTEM.VAL(r, 3);
>              ^

"eExpr.m", line 82: 'k' has not been declared
>   x := x IS k
>             ^
>>*)
