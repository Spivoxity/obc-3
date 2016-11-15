MODULE Main;

IMPORT Sleep, Out, Files;

VAR ans: INTEGER;

BEGIN
  Out.String("Thinking..."); Files.Flush(Files.stdout);
  ans := Sleep.Usec(2000000);
  Out.String("done"); Out.Ln;
  Out.String("The answer is "); Out.Int(ans, 0); Out.Ln
END Main.
