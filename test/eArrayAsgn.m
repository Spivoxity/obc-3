MODULE eArrayAsgn;

PROCEDURE p(a: ARRAY OF INTEGER);
  VAR b: POINTER TO ARRAY OF INTEGER;
BEGIN
  b^ := a
END p;

END eArrayAsgn.

(*<<
"eArrayAsgn.m", line 6: types do not match exactly on the RHS of this
assignment
>   b^ := a
>         ^
> This expression has type ARRAY OF INTEGER
>>*)
