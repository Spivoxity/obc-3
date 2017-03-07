MODULE eTypes;

TYPE aaa = bbb; bbb = INTEGER;

TYPE ccc = ARRAY 10 OF ARRAY OF INTEGER;

PROCEDURE p*(a: ARRAY 10 OF ARRAY OF INTEGER); END p;

END eTypes.

(*<<
"eTypes.m", line 3: 'bbb' has not been declared
> TYPE aaa = bbb; bbb = INTEGER;
>            ^^^

"eTypes.m", line 5: arrays cannot have open arrays as elements
> TYPE ccc = ARRAY 10 OF ARRAY OF INTEGER;
>                        ^^^^^^^^^^^^^^^^

"eTypes.m", line 7: arrays cannot have open arrays as elements
> PROCEDURE p*(a: ARRAY 10 OF ARRAY OF INTEGER); END p;
>                             ^^^^^^^^^^^^^^^^

"eTypes.m", line 7: warning -- you should name this parameter type, or else
no actual parameter will match
> PROCEDURE p*(a: ARRAY 10 OF ARRAY OF INTEGER); END p;
>                 ^^^^^^^^^^^^^^^^^^^^^^^^^^^^
>>*)
