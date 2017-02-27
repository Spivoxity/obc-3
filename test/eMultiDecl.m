MODULE eMultiDecl;

VAR x: INTEGER;
VAR x, y: REAL;

TYPE t = INTEGER; t = INTEGER;

BEGIN
  y := x
END eMultiDecl.

(*<<
"t/eMultiDecl.m", line 4: 'x' has already been declared
> VAR x, y: REAL;
>     ^

"t/eMultiDecl.m", line 6: 't' has already been declared
> TYPE t = INTEGER; t = INTEGER;
>                   ^
>>*)
