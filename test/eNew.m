MODULE eNew;

VAR f: POINTER TO RECORD END;
  g: POINTER TO ARRAY OF INTEGER;

BEGIN
  NEW(g, FALSE);
  NEW(g);
  NEW(f, 3);
  NEW(NIL)
END eNew.

(*<<
"t/eNew.m", line 7: type INTEGER is needed as bound of NEW
>   NEW(g, FALSE);
>          ^^^^^
> This expression has type BOOLEAN

"t/eNew.m", line 8: NEW expects 2 arguments
>   NEW(g);
>   ^^^

"t/eNew.m", line 9: NEW expects 1 argument
>   NEW(f, 3);
>   ^^^

"t/eNew.m", line 10: the argument of NEW must be a pointer variable
>   NEW(NIL)
>       ^^^
> This expression has type NIL
>>*)
