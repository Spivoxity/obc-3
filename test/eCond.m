MODULE eCond;

VAR x: INTEGER;

BEGIN
  IF 3 THEN END;
  WHILE NIL DO END;
  REPEAT ;;; UNTIL x
END eCond.

(*<<
"eCond.m", line 6: the test in an IF statement must have type BOOLEAN
>   IF 3 THEN END;
>      ^
> This expression has type integer

"eCond.m", line 7: the test in a WHILE statement must have type BOOLEAN
>   WHILE NIL DO END;
>         ^^^
> This expression has type NIL

"eCond.m", line 8: the test after UNTIL must have type BOOLEAN
>   REPEAT ;;; UNTIL x
>                    ^
> This expression has type INTEGER
>>*)
