MODULE eBuiltin;

PROCEDURE fz(f: PROCEDURE (a, b: INTEGER): INTEGER); END fz;

BEGIN
  fz(ASH)
END eBuiltin.

(*<<
"eBuiltin.m", line 6: built-in procedure 'ASH' may not be used as a procedure
value
>   fz(ASH)
>      ^^^
>>*)
