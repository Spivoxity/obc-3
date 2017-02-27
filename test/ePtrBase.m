MODULE ePtrBase;

TYPE t1 = POINTER TO t2; t2 = INTEGER;

END ePtrBase.

(*<<
"t/ePtrBase.m", line 3: the base type of a pointer must be an array or record
> TYPE t1 = POINTER TO t2; t2 = INTEGER;
>                      ^^
> This expression has type INTEGER
>>*)
