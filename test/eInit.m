MODULE eInit;

TYPE M = POINTER TO RECORD END;
  T = POINTER TO Tree;

PROCEDURE E;
  VAR a : T; m : M;
BEGIN
  NEW(a, m^.x);
END E;

END eInit.

(*<<
"eInit.m", line 4: 'Tree' has not been declared
>   T = POINTER TO Tree;
>                  ^^^^
>>*)
