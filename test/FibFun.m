MODULE FibFun;

CONST name* = "fib";

PROCEDURE Fib*(n: INTEGER): INTEGER;
  VAR a, b, c, i: INTEGER;
BEGIN
  IF n = 0 THEN
    RETURN 0
  ELSE
    a := 0; b := 1; i := 1;
    WHILE i < n DO
      c := a + b; a := b; b := c;
      i := i + 1
    END;
    RETURN b
  END
END Fib;

END FibFun.
