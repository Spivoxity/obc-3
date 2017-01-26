MODULE eLen;

PROCEDURE depth*(a: ARRAY OF ARRAY OF INTEGER): INTEGER;
BEGIN
  RETURN LEN(a, 2)
END depth;

END eLen.

(*<<
"./eLen.m", line 5: the argument of LEN must be an array of at least 3
dimensions
>   RETURN LEN(a, 2)
>              ^
> This expression has type ARRAY OF ARRAY OF INTEGER
>>*)
