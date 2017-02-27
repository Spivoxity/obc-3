MODULE eWith;

VAR x: INTEGER;

BEGIN
  WITH x: REAL DO END
END eWith.

(*<<
"t/eWith.m", line 6: a record pointer or VAR parameter is needed here
>   WITH x: REAL DO END
>        ^
> This expression has type INTEGER
>>*)
