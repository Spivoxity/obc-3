MODULE eYacc;

(* A yacc bug prevented error recovery in the following case *)

PROCEDURE THEN; END p;

END eYacc.

(*<<
"eYacc.m", line 5: syntax error at token 'THEN'
> PROCEDURE THEN; END p;
>           ^^^^
>>*)
