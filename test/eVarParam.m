MODULE eVarParam;

IMPORT Out;

CONST s = 'x';

PROCEDURE p(VAR c: CHAR);
BEGIN
  Out.Char(c); Out.Ln
END p;

BEGIN
  p(s)
END eVarParam.

(*<<
"eVarParam.m", line 13: VAR parameter 'c' should be a variable
>   p(s)
>     ^
>>*)
