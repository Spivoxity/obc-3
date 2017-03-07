MODULE eStruct;

TYPE
  tt1 = ARRAY 2 OF POINTER TO tt1;
  tt2 = ARRAY 2 OF POINTER TO tt2;

VAR xx1: tt1; xx2: tt2;

BEGIN
  xx1 := xx2
END eStruct.

(*<<
"eStruct.m", line 10: types do not match exactly on the RHS of this
assignment
>   xx1 := xx2
>          ^^^
> This expression has type tt2 = ARRAY 2 OF POINTER TO ARRAY 2 OF POINTER TO
> ARRAY 2 OF ...
>>*)
