MODULE eBadTypes;

VAR x: INTEGER;

TYPE t3 = Types.Name; t4 = x.type; t5 = x;

END eBadTypes.

(*<<
"t/eBadTypes.m", line 5: module 'Types' has not been imported
> TYPE t3 = Types.Name; t4 = x.type; t5 = x;
>           ^^^^^^^^^^

"t/eBadTypes.m", line 5: 'x' is not a module
> TYPE t3 = Types.Name; t4 = x.type; t5 = x;
>                            ^^^^^^

"t/eBadTypes.m", line 5: 'x' is not a type
> TYPE t3 = Types.Name; t4 = x.type; t5 = x;
>                                         ^
>>*)
