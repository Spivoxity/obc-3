MODULE eCase;

VAR x: INTEGER;

BEGIN
  CASE x OF 3..2: | 1: | 1: | 'a': END;
  CASE 'x' OF 'a': | 'a': END;
  CASE TRUE OF TRUE: END
END eCase.

(*<<
"t/eCase.m", line 6: this case label specifies an empty range
>   CASE x OF 3..2: | 1: | 1: | 'a': END;
>             ^^^^

"t/eCase.m", line 6: a case label should have type INTEGER
>   CASE x OF 3..2: | 1: | 1: | 'a': END;
>                               ^^^
> This expression has type CHAR

"t/eCase.m", line 7: this is a duplicate case label for value 'a'
>   CASE 'x' OF 'a': | 'a': END;
>                      ^^^

"t/eCase.m", line 8: CASE expects an integer or character expression
>   CASE TRUE OF TRUE: END
>        ^^^^
> (Use the -x flag to remove this restriction)
> This expression has type BOOLEAN
>>*)
