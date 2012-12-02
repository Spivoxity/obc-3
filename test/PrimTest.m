MODULE PrimTest;

IMPORT Out;

VAR x: INTEGER;

PROCEDURE Foo(x: INTEGER) IS "PrimFoo";
(* CODE printf("foo: %d\n", arg[1].i); *)

PROCEDURE Baz(): INTEGER IS "PrimBaz";
(* CODE result.i = 42; *)

TYPE Handler = PROCEDURE (x: INTEGER): INTEGER;

PROCEDURE Bar(x: INTEGER): INTEGER;
BEGIN
  Out.String("bar: "); Out.Int(x, 0); Out.Ln;
  RETURN 66
END Bar;

PROCEDURE Bip(h: Handler): INTEGER IS "PrimBip";
(* CODE result = callback(arg[1].p, 1, 34); *)

PROCEDURE Dip(h: Handler): INTEGER IS "PrimDip";
(* CODE result = obcall(arg[1].p, cp, bp, sp, 1, 35); *)

BEGIN
  Foo(17);
  x := Bar(Baz());
  x := Bar(Bip(Bar));
  x := Bar(Dip(Bar))
END PrimTest.
