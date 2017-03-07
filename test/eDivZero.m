MODULE eDivZero;

CONST N = 23; Y = N / (N-N);

END eDivZero.

(*<<
"eDivZero.m", line 3: this expression divides a constant by zero
> CONST N = 23; Y = N / (N-N);
>                   ^^^^^^^^^
>>*)
