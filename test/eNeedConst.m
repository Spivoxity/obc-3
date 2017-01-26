MODULE eNeedConst;

VAR x: INTEGER;

CONST xx = x;

END eNeedConst.

(*<<
"./eNeedConst.m", line 5: a CONST declaration must contain a constant
> CONST xx = x;
>            ^
>>*)
