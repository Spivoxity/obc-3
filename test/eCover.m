MODULE eCover;

(* Test coverage *)

BEGIN
  EXIT;
  WITH x: t DO END
END eCover.

(*<<
"eCover.m", line 6: this EXIT statement is not inside a LOOP statement
>   EXIT;
>   ^^^^

"eCover.m", line 7: 'x' has not been declared
>   WITH x: t DO END
>        ^
>>*)
