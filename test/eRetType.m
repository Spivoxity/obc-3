MODULE eRetType;

TYPE rec = RECORD END;

PROCEDURE foo*(x: INTEGER): INTEGER;
BEGIN
  RETURN;
  RETURN 'c'
END foo;

PROCEDURE baz*(x: INTEGER);
BEGIN
  RETURN 7
END baz;

PROCEDURE pip*(): rec; END pip;

BEGIN
  pip();
  RETURN 2
END eRetType.

(*<<
"t/eRetType.m", line 16: a procedure may not return an array or record type
> PROCEDURE pip*(): rec; END pip;
>                   ^^^
> This expression has record type 'rec'

"t/eRetType.m", line 7: this RETURN statement should specify a result
>   RETURN;
>   ^^^^^^

"t/eRetType.m", line 8: type INTEGER is needed in this RETURN statement
>   RETURN 'c'
>          ^^^
> This expression has type CHAR

"t/eRetType.m", line 13: this RETURN statement should not specify a result
>   RETURN 7
>   ^^^^^^^^

"t/eRetType.m", line 16: warning -- this typed procedure returns no result
> PROCEDURE pip*(): rec; END pip;
>           ^^^

"t/eRetType.m", line 19: a call that returns a result cannot be used as a
statement
>   pip();
>   ^^^^^
> (Use the -x flag to remove this restriction)

"t/eRetType.m", line 20: a RETURN statement is not allowed in a module body
>   RETURN 2
>   ^^^^^^^^
>>*)
