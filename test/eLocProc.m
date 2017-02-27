MODULE eLocProc;

PROCEDURE mopsy*;
  PROCEDURE pop; END pop;
  VAR p: PROCEDURE;
BEGIN
  p := pop
END mopsy;

END eLocProc.

(*<<
"t/eLocProc.m", line 7: local procedure 'pop' may not be used as a procedure
value
>   p := pop
>        ^^^
>>*)
