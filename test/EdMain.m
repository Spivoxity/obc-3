MODULE EdMain;

(* pp/ewoks/EdMain.m *)

(* A testing framework for the editor.  Since the editor is far too
   unreliable to be used for real work, we read the fixed file
   \fn{foo} and write the modified text to a file named~\fn{baz}. *)

IMPORT Files, Terminal, Plane, Display, Editor, Keymap, Err;

VAR f: Files.File; k: Terminal.key;

BEGIN
  f := Files.Open("foo", "r");
  IF f = NIL THEN
     Err.String("Can't read 'foo'"); Err.Ln;
     HALT(1)
  END;
  Plane.buf.InsertFile(f);
  Files.Close(f);
  Plane.Init;
  Display.Refresh;

  WHILE Editor.active DO
    Terminal.GetKey(k);
    Keymap.DoCommand(k)
  END;

  f := Files.Open("baz", "w");
  Plane.buf.WriteFile(f);
  Files.Close(f);
  Terminal.Goto(1, Terminal.HEIGHT);
  Terminal.ClearLine
END EdMain.
