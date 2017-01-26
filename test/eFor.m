MODULE eFor;

CONST N = 2;
VAR x: INTEGER; y: REAL;

BEGIN
  FOR N := 2 TO 3 DO END;
  FOR y := 2 TO 3 DO END;
  FOR x := 2.0 TO 3.0 DO END;
  FOR x := 2 TO 3 BY y DO END;
  FOR x := 2 TO 3 BY 0 DO END
END eFor.

(*<<
"./eFor.m", line 7: the name after FOR must be a variable
>   FOR N := 2 TO 3 DO END;
>       ^

"./eFor.m", line 8: the variable after FOR must have a discrete type
>   FOR y := 2 TO 3 DO END;
>       ^
> This expression has type REAL

"./eFor.m", line 9: type INTEGER is needed as a starting value
>   FOR x := 2.0 TO 3.0 DO END;
>            ^^^
> This expression has type REAL

"./eFor.m", line 9: type INTEGER is needed as an ending value
>   FOR x := 2.0 TO 3.0 DO END;
>                   ^^^
> This expression has type REAL

"./eFor.m", line 10: a step value must be a constant
>   FOR x := 2 TO 3 BY y DO END;
>                      ^

"./eFor.m", line 10: the step value must be an integer
>   FOR x := 2 TO 3 BY y DO END;
>                      ^
> This expression has type REAL

"./eFor.m", line 11: the step value must be non-zero
>   FOR x := 2 TO 3 BY 0 DO END
>                      ^
>>*)
