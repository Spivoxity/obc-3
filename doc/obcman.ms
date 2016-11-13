\input mymacs
\stretchpage

%% Title page
\blanktrue
\null
\vskip60pt
\vbox to24pt{\vfill \centerline{\twentysf Oxford Oberon--2 Compiler}}
\vskip24pt
\vbox to30pt{\vfill \centerline{\twentyfourfunky Users' Manual: version 3.0}}
\vskip24pt
\vbox to24pt{\vfill \centerline{\twentysf Mike Spivey}}
\vfill
\rightline{Oxford University Computing Laboratory}
\bigskip
\rightline{Draft of \number\day.\romannumeral\month.\number\year}
\bigskip
\rightline{Copyright \copyright\ 1999--2016 J. M. Spivey}
\clearpage

\chapter* Contents.
\thecontents

\chapter Introduction.

This manual explains how to use the Oxford Oberon--2 compiler \pn{obc}
to compile and run Oberon--2 programs.
The content is largely extracted from the laboratory manual used by
our undergraduates in Oxford for their second programming course
(their first course is functional programming with Haskell).

The following typographical conventions are used throughout this manual:
\itemize
\item Names of programs are shown in \pn{bold face} type.
\item File names are shown in \fn{italic} type.
\item The text of Oberon programs is shown in {\sf sans serif} type,
except that identifiers in the running text are shown in |italic|.
\item The text of \unix\ commands is shown in \verb/typewriter/ type.
Where an interaction with the computer is shown, the characters you
type are shown in {\tti italic typewriter\/} type, and the computer's
responses are shown in {\tt upright typewriter} type.
\enditems

%\input usingobc
\chapter Using Oberon.

This chapter will tell you how to use the Oberon compiler \pn{obc} to
compile and run Oberon programs.

\section Introducing the Oberon compiler.

Before your Oberon program can be run, it must first be translated by
the Oberon compiler into a form that can be directly obeyed by a
machine.  For example, Figure~\ref{f:Fac.m} shows a little Oberon
program for computing factorials.  As you can see, the name of the
file exactly matches the name of the module |Fac| that it
contains, even down to the capitalization of the name.
(In the software lab at Oxford, you can find a copy of this 
file at {\vsf /usr/local/practicals/ip1\allowbreak/examples/Fac.m}.)
\figure
\program
MODULE Fac;
IMPORT In, Out;

VAR n, i, f: INTEGER;

BEGIN
  LOOP
    Out.String("Gimme a number: "); In.Int(n);
    IF n < 0 THEN EXIT END;

    i := 0; f := 1;
    WHILE i < n DO i := i+1; f := f*i END;

    Out.String("The factorial of "); Out.Int(n, 0); 
    Out.String(" is "); Out.Int(f, 0); Out.Ln
  END
END Fac.
\endprog
\caption{Contents of file \fn{Fac.m}}
\label{f:Fac.m}
\endfig
To translate this program with the Oberon compiler, give the
command\label{p:obc}%
\footnote{In this manual, I've shown the shell's prompt as \verb/$/,
but it may appear differently, depending on the shell that your
account has been set up with.  The characters you type are shown in
{\tti slanted type}.}
\interact
$ _obc -o fac Fac.m_
\endinter
This command runs the Oberon compiler \pn{obc}, asking it to
translate the program found in \fn{Fac.m} and place the translation
(``\verb/-o fac/'') in the file \fn{fac}.%
\footnote{If you don't say ``\verb/-o fac/'', the compiler rather
unhelpfully puts the translation in a file called \fn{a.out},
according to a long-standing \unix\ tradition.}

When you have done this, you can run the program by giving the \unix\
command \verb:./fac:, like this:
\interact
$ _./fac_
Gimme a number: _4_
The factorial of 4 is 24
Gimme a number: _5_
The factorial of 5 is 120
Gimme a number: _6_
The factorial of 6 is 720
Gimme a number: _-1_
$
\endinter
As you can see in Figure~\ref{f:Fac.m}, the program contains a loop
that repeatedly asks for a number and computes its factorial, until
a negative number is input.  With our usual setup at the Computing
Laboratory, you have to type \verb:./fac: as the \unix\ command for
starting \pn{fac}, to reflect the fact that the file \fn{fac} is
in the 
current directory, not one of the directories where \unix\ usually
keeps executable programs.%
\footnote{I find this need to type \verb:./fac: instead of just
\verb/fac/ very annoying.
If you hate it as much as I do, ask the demonstrator how to change it,
after asking about the tiny security risk involved.}

The file \fn{fac} contains low-level instructions that are
equivalent to the high-level program in \fn{Fac.m}.  Once the
translation has been done, you could delete the file \fn{Fac.m} and
still run the low-level version in \fn{fac}; or you could copy
the file \fn{fac} and give it to a customer, who could run the program
without needing to have the source code.

There's\label{p:JIT} something a little unusual about the Oberon
compiler we'll be using, and that's the fact that (like most compilers
for Java) it doesn't actually translate your 
program into instructions for the physical computer you are using.
Instead, the designer of the compiler has invented a simple
`virtual' computer specially for implementing Oberon, and the file
\fn{fac} contains  instructions for that computer instead.  The gap
between the virtual computer and the actual, physical computer is
bridged in one of two ways: either by means of an {\it interpreter\/}
program that uses the physical computer to carry
out a simulation of the virtual computer, or by means of a {\it
just-in-time translator\/} (JIT) that compiles code for the physical
machine after the program is loaded, but before each piece of it
starts to run. 
The biggest advantage of using a virtual machine 
is that the compiler can be {\it ported\/} to a new computer
just by making a new interpreter -- and since the interpreter is itself
written in a highish-level language (C), that's usually just a matter
of re-compiling it on the new computer.
JIT translators must be designed separately for each machine
architecture, though they may share common parts.  At present,
translators exist for the x86 architecture used by PCs and Macs and
for the ARM architecture used by the Raspberry Pi.

The biggest disadvantage of this scheme is that programs run more
slowly than they would with a conventional compiler.  The
interpreter-based implementation is perhaps 5 to 10 times slower than
a code from a conventional compiler, whilst the JIT is also slower,
because it generates code in a simple way without a lot of optimization.
But we won't be writing any programs that run for a very long time, so
that won't be too much of a disadvantage.%
\footnote{Actually, there's a hidden advantage: having an artificially
slowed-down computer will encourage you to learn how to write programs
that make economical use of computer time.}

\section Programs with more than one module.

Bigger programs are often divided into several modules for ease of
understanding.  If this division into modules is done judiciously, it
should be possible to understand a lot about each module without
looking at the other ones.  A well-designed module has a small
interface, and it is possible to say {\em what\/} the module does
without giving the details of {\em how\/} it does it.

As a contrived example, let's look at a variation on our program for
calculating factorials that calculates Fibonacci numbers instead.
Calculating Fibonacci numbers is a simple task that can be done in
several ways, some more complicated and efficient than others.  That
makes it sensible to introduce a procedure, so as to separate the {\em
what\/} -- the Fibonacci function -- from the {\em how\/} -- the
algorithm for calculating it.

Putting the procedure in its own module is over-kill, since there are
no details to hide that are not already hidden inside the procedure.
But doing so will let us show how to split a program into modules, so
let's do it anyway.  Figure~\ref{f:FibMain.m} shows a program like our
earlier factorials program, but with the code to calculate the
factorial replaced by a call to the function {\em FibFun.Fib},
imported from a module {\em FibFun}.
Figure~\ref{f:FibFun.m} shows the text of that module, including a
procedure that calculates Fibonacci numbers.  Note the export mark
{\vsf *} in the heading of this procedure.

\figure
\program
MODULE FibMain;
IMPORT In, Out, FibFun;

VAR n, i, f: INTEGER;

BEGIN
  LOOP
    Out.String("Gimme a number: "); In.Int(n);
    IF n < 0 THEN EXIT END;

    f := FibFun.Fib(n);

    Out.String("fib("); Out.Int(n, 0); 
    Out.String(") = "); Out.Int(f, 0); Out.Ln
  END
END FibMain.
\endprog
\caption{Contents of file \fn{FibMain.m}}
\label{f:FibMain.m}
\endfig

\figure
\program
MODULE FibFun;
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
\endprog
\caption{Contents of file \fn{FibFun.m}}
\label{f:FibFun.m}
\endfig

To compile this program, we must separately translate the two modules
into machine code, then combine the two files of machine code into one
program.  Although all this can be done with the single command
\interact
$ _obc -o fib FibFun.m FibMain.m_
\endinter
it is better to separate the three steps, so that we need not repeat
all the steps if we change only some of the modules.
With a small example like this,
it doesn't make a significant difference: but as I've already
admitted, using modules at all in this example is over-kill.
To compile the two modules separately, we need the three commands
\interact
$ _obc -c FibFun.m_
$ _obc -c FibMain.m_
$ _obc -o fib FibFun.k FibMain.k_
\endinter
The first command translates the |FibFun| module, producing a
file \fn{FibFun.k} that contains the virtual machine code, and also
some information about the exported interface of the module.
The second command translates the |FibMain| module into a file
\fn{FibMain.k} of virtual machine code; during
the translation, the compiler reads the file \fn{FibFun.k} to check
that the function {\em FibFun.Fib\/} is used in a way that is
consistent with its definition.
The third command combines the two files of machine code with the code
for the library modules |In| and |Out| to produce the
executable file \fn{fib}.

The files \fn{FibFun.k} and \fn{FibMain.k} containing the low-level
instructions are in a textual format, so if you are curious you can
examine them with a text editor.
Don't try changing them, though: that will probably
result in chaos.
You will see that the files contain lines from your source program in
the form of comments.  These are completely ignored by the simulator,
but are there to help us track down problems with the Oberon compiler.

\section Managing compilation with make.

If you are writing a program, perhaps in several modules, and trying
to get it to work, it becomes 
very boring to have to type out a long sequence of commands each time
you want to compile it.  
Also, if we make a change to
only one of the modules in a big program, we want to avoid recompiling
all of it so as to save time.

A partial solution to this problem would be to write a {\em shell
script\/} for compiling the program: that is, a sequence of \unix\
commands that is stored in a file.
This solves the problem of typing a long command or sequence of
commands each time we want to compile our program; but there's a
better way that uses a nifty program called \pn{make}.  This program
lets you write a script that, like a shell script, contains the \unix\
commands for compiling your program.  Unlike a shell script, the
\pn{make} script associates each of these commands with a file that
it produces.  The \pn{make} program uses the modification time that
\unix\ associates with each file to work out which files in your program
have been changed, and issues only the commands that are needed to
bring everything up to date.  Usually, there's just one script that
describes how to compile all the programs in a directory, and the
script is stored in the same directory under the name \fn{Makefile}.

\figure
\listing
# Makefile for fib

FIB = FibFun.k FibMain.k
fib: $(FIB)
        obc -o fib $(FIB)

FibMain.k: FibFun.k

%.k: %.m
        obc -c $<
\endlist
\caption{Contents of file \fn{Makefile}}
\label{f:Makefile}
\endfig

For example, Figure~\ref{f:Makefile} shows a makefile for our little
Fibonacci program: I've numbered the lines for ease of reference.
In this course, you will not need to know how to write makefiles,
because a working makefile
will be provided 
when it is needed.  Nevertheless, \pn{make} is
such a useful program that it's good to understand something of what
it can do.

Line 1 of the makefile is a comment: it starts with \verb/#/ and
continues to the end of the line.
Lines 3--4 describe the last of the three commands needed to build the
\pn{fib} program, using the list of files \verb/FIB/ defined on
line~2.  Line 3 records the fact that the file \fn{fib} is made from
the two files \fn{FibFun.k} and \fn{FibMain.k}, so that it should
be rebuilt if either of these two files is newer than it; and line 4
(starting with a tab character) gives the \unix\ command to execute in
this case.

The other two commands needed to build \pn{fib} are summarized in
the `pattern rule' on lines 
6--7; this says that any file \fn{???.k} can be made from the
corresponding file \fn{???.m} by running the command \verb/obc -c ???.m/.
Line 5 records the fact that rebuilding \fn{FibFun.k} requires
\fn{FibMain.k} to be rebuilt too, because the interface of the
|FibFun| module may have changed.  

Suppose we were to modify our program,
perhaps to make it more acceptable to English tastes 
by replacing the word ``{\sf Gimme}'' by the polite
phrase ``{\sf Kindly enter}''.  
To do this, we would use a text editor on the file \fn{FibMain.m}, and
our final action with the editor would be to save the new file over
the old one.  This would cause \unix\ to change the
modification time of the file, making it newer than the file
\fn{FibMain.k} that was generated by the compiler last time we built
the program.
Now we simply give the command
\interact
$ _make_
\endinter
The \pn{make} program examines the rule that says how to make
\fn{FibMain.k} from \fn{FibMain.m} and sees that the time-stamps
are out of step.
So it
runs the Oberon compiler and creates an up-to-date version of
\fn{FibMain.k}.
This makes \fn{FibMain.k} newer than the final program \fn{fib},
so \pn{make} also runs the command to rebuild \fn{fib}.  Since we
haven't touched the |FibFun| module, it does not need to be rebuilt,
and \pn{make} omits the command to build it.
If we immediately run \pn{make} again, it will do nothing the second
time, because each file will already be up to date with respect to the
files from which it is built.  So it's quick and easy to use \pn{make}
simply to check that everything is up to date.

Two very common mistakes in using editors and \pn{make} are these:
forgetting to save a file in the editor before running \pn{make}, and
forgetting to run \pn{make} after changing your program.  The first
kind of mistake is noticeable, because \pn{make} does less work than
you expected: if you change a source file, it ought to be recompiled.
Both kinds of mistake become noticeable when your program continues
behaving exactly as it did before you made your changes.

\section When things go wrong.

There are many kinds of things that can go wrong in writing a program:
\itemize
\item You might write a program that does not conform to the
grammatical rules of Oberon.
\item You might write a program that, although grammatically correct,
does not make any sense because you have (for example) forgotten to
declare some of the variables you use.
\enditems
Because of the way the Oberon compiler has been written, it checks
that your program is grammatically correct before it begins to address
the question of whether the program makes sense, and it checks the
whole program for sense before it starts to generate the machine code
translation. So if you write a grammatically incorrect program that
also contains undeclared identifiers, then the compiler will not
bother to mention the missing declarations until you have put the
grammar right; and if the program contains grammatical errors, 
if declarations are missing or if the types do not match 
up properly in expressions, the compiler will not generate any machine
code for your program until you have put it all right.\footnote{This
is a bit annoying when the only error is a missing semicolon, I admit.}

After detecting one grammatical error in your program, the compiler
tries to continue with its analysis of the rest of your program, so
that you can correct more than one error each time you run the
compiler.  The compiler works by reading your program from beginning
to end, and it produces its {\em first\/} error message when it
reaches the first word or symbol in your program that could not be the
next symbol in any valid Oberon program. %
%\footnote{It's a bit like the
%game where each player must add another word to a growing sentence:
%the loser is the first person to say a word that couldn't possibly
%fit in any sentence.}  
So the first error message at least is reliable.

After the first error, the compiler has the problem of re-construing
your program in such a way that it can continue and look for other errors.
The methods used for this are necessarily crude, because
after all your program is wrong, and the compiler can only guess at
what you meant.
The compiler uses rules for recovery like ``skip to the next
semicolon, and see if what follows looks like the beginning of another
statement''.  This means that the compiler can diagnose as another
error in your program a problem that is in fact caused by the
compiler's own state of confusion.
For example, the compiler might skip over an |END|
keyword as it looks for a place to restart, and that might cause it to
think that a later procedure declaration appears in the middle of the
current procedure.  There's no way to avoid this problem in general,
so it's best to be aware that only the first error message is really
worth relying on.

When errors are reported by the compiler, the combination of the
\pn{obc} compiler, \pn{make} and the \pn{emacs} editor is particularly
productive.  \pn{Emacs} and its X version \pn{xemacs} provide a
command that you can run by using the menus or toolbar under X, or by
typing \verb/M-x compile/;%
\footnote{Hold down the Alt key and press \verb/x/, then
release both, type the word \verb/compile/, and press Return.}
this uses \pn{make} to recompile your program, and displays the output
of that process, including any messages from the compiler, in an
\pn{Emacs} buffer.  The \pn{obc} compiler puts its error messages in
a standard format, like this:
\verbatim
"Planner.m", line 92: missing ';' at token 'IF'
>       IF verbose THEN ShowLink(u, "green") END;
>       ^^
\endverb
Another \pn{emacs} command
\verb/next-error/ (bound to the key sequence\footnote{Hold
down the control key while pressing letter \verb/x/, then release both
and press \verb/`/ (grave accent).}
%Now take your left foot, pass it
%behind your right knee, and put the big toe in your right hip
%pocket \dots%
\verb/C-x `/) recognizes these error messages and
finds the relevant file and the relevant line, putting the editor's
cursor there ready to correct the error.  Subsequent invocations of
\verb/next-error/ will take you to the locations of other errors
reported by the compiler.

Other things can go wrong when you run your program:
\itemize
\item You might write a program that performs an illegal operation
when it runs.  Examples of such illegal operations are accessing an
array with an index that is negative or greater than the length of the
array, and following a pointer that has been set to |NIL|.
\enditems
Errors like these are detected when the program runs, because there is
no generally applicable way for the compiler can work out in advance
whether such errors can happen.\footnote{Actually, we can {\em prove
mathematically\/} that no compiler could do this, because it is
equivalent to solving the `halting problem' for Turing machines.  The
intriguing theory of such things is covered in the second year course {\it Models of Computation}.}
The advantage of a language like Oberon is that the compiler can
generate code that checks for such errors, and stops the program
immediately.
For an array reference |a[i]|, the compiler generates a
check that |0 <= i < N|, where |N| is the length of |a|.  If this is
not true, then the program is stops with a
message such as
\verbatim
Runtime error: array bound error 
                on line 123 in module Directory
\endverb
The Oberon language has been very cleverly designed so that errors of
this kind can be detected immediately with simple compiler-generated
checks.  This property does not hold for C, for example, so C
programmers are always trying to track down the cause of baffling
problems.\footnote{That's why debuggers are so popular
with C programmers: when a program crashes, they use a debugger to try
and work out what has happened.}

Some things that can go wrong do not result in any helpful message:
\itemize
\item Your program may run forever, without producing any output.
A common cause of this is that you've written a loop
\quotation
\vsf WHILE i < n DO ...
\endquote
but have forgotten to put |i := i + 1| in the loop body.

\item Your program may produce output that is wrong.  In this case,
there's no substitute for careful thought, but you can gather evidence
about what is happening either by adding code at suitable points in
the program that prints relevant information about the value of
variables, or by using a debugger (see the next section).

\item Your program may produce the right output, but run too slowly to
be useful.  In this case, the solution is {\em not\/} to start madly
`optimizing' everything in sight in the hope of speeding things up.
If you do that, you will probably spend most of your effort on parts
of the program that have almost no effect on the overall speed, and
you will certainly make your program more complicated, perhaps so much
so that it stops working properly, and you end up in a worse position
than when you started.  A better alternative is to use {\em
profiling\/} to find out where the program is spending its time: see
Section~\ref{s:profiling}.
\enditems

\section A debugger.
\label{s:debugger}

If a program produces the wrong output, then it's often hard to see
from the output what could be going wrong.  In order to investigate
further, you can take some steps for yourself.  Adding assertions to
the program, particularly at the start of procedures, can help to pin
down where the fault lies.  You can also add printing code to the
program that gives a kind of running commentary on what the program is
doing.  If the program has significant internal data structures, then
it is well worth the effort of writing a general, robust print routine
that prints out those data structures in human-intelligible form.
A convenient alternative to instrumenting the program by hand is to
use a debugger, a utility program that allows you to watch and control
the actions of your program as it runs.

\figure
\ifpdf
\pdfximage width\hsize{debugger.png}
\hbox{\pdfrefximage\pdflastximage}
\else
\epsfxsize=\hsize
\hbox{\epsfbox{debugger.eps}}
\fi
\caption{Oberon debugger}
\label{f:obdb}
\endfig

To start the debugger, use a command like
\interact
$ _obdb ./total_
\endinter
where \fn{total} is a compiled Oberon program.
Any arguments for the program can appear after the name of the program
on the command line.

The debugger shows a window like \longref{Figure}{f:obdb}.
On the left, you see a listing of the source code; the margin shows
line numbers and a little triangle that indicates the next line to be
executed.  You can step through the program line by line by clicking
on one of the three buttons `Step into' (which stops inside any
procedure that is called), `Step over' (which continues until the next
line of the current procedure) or 'Step out' (which continues until
the current procedure returns). 
If the program consists of more than one module, then the 'notebook'
of source listings will have several pages.

At the top right, you see a representation of the subroutine stack of
the program.  Since the program shown here contains no procedures,
there is only one significant entry, the subroutine
\verb@L1Total.%main@ that represents the body of the module~|L1Total|.
By clicking on different entries in the list, you can view the source
code for different procedures in the left-hand pane, with the current
location highlighted with the yellow bar. The little blue triangle
always shows the current execution point, even if the yellow bar has
been moved to another procedure.

At the bottom right, the local variables for the currently selected
procedure are shown.  Arrays and records are initially shown in a
`closed' state, with just the first few elements visible on one line,
but you can open them by clicking on the little triangle, and see the
contents listed with each element on a separate line.  The display can
be changed to different active procedures by clicking in the stack
display.

If you click in the left margin of the source display, you can set and
clear breakpoints, which will stop the execution of the program when
they are reached.  If you click on a line which does not correspond
to any actual instructions in the program, then the breakpoint may be
created a couple of lines further down.  Breakpoints are shown by
a red circle, and execution always stops at a breakpoint, even if it
was started with the `Step over' or `Step out' button and would
otherwise continue further.

\section Profiling.
\label{s:profiling}

Profiling is a process of gathering statistics about where a program
spends its time, as the first step of speeding it up.  Profiling makes
for more effective optimization, because it lets you concentrate your
effort on the things that will really make a difference.  In most
large programs, the majority of the code accounts for only a small
fraction of the execution time -- or to put it another way, a small
part of the code consumes a big fraction of the time.  If you can
identify this small part of the code -- sometimes called the 
{\em inner loop}, then you can devote all your
energies to improving it.  Perhaps there is a better algorithm or data
structure that will speed up the time-consuming task.  Or perhaps a
different approach to the whole program will make the problem area go
away.  Time spent on optimizing the rest of the program, apart from
the inner loop, may be entirely wasted, because the effect on the
overall speed may be negligible.

Our Oberon system provides a profiling tool called \pn{obprof} that is
used as follows: if you would normally run your program through the
command
\interact
$ _prog arg1 ... argn_
\endinter
then you should use the command\label{p:obprof}
\interact
$ _obprof prog arg1 ... argn_
\endinter
instead.\footnote{In the Windows version, you have to type something
like 
\interact
C:\> _obprof prog.exe arg1 ... argn_
\endinter
and specify the \verb/.exe/ suffix explicitly.}
The {\em obprof\/} program is actually an augmented version of
the usual Oberon run-time system; it runs your program exactly as
usual, but it keeps statistics about what happens as the program
runs.  Specifically, {\em obprof\/} keeps track of what procedure in your
program is active, and measures how much time is spent in each procedure.
When your program finishes, {\em obprof\/} summarizes the information it
has gathered by listing the procedures in your program, how often each
one was called, and the total time spent in the procedure.
Figure~\ref{f:profile} shows the output that is produced for a trial
run of the \pn{fac} program.
\figure
\verbatim
Execution profile:

    Ticks    Frac     Cumul   Calls   Procedure
 --------------------------------------------------
      174    31.4%    31.4%       2   In.Int
      155    28.0%    59.4%       1   Fac.%main
       78    14.1%    73.5%       6   In.Char
       56    10.1%    83.6%       7   In.IsDigit
       28     5.1%    88.6%       4   Out.String
       24     4.3%    93.0%       2   In.IsSpace
       14     2.5%    95.5%       2   Out.Int
       10     1.8%    97.3%       1   %Main.%main
        7     1.3%    98.6%       1   Files.%main
        5     0.9%    99.5%       1   Out.Ln
        3     0.5%   100.0%       1   In.%main
        0     0.0%   100.0%       2   Conv.IntVal
        0     0.0%   100.0%       6   Files.Eof
        0     0.0%   100.0%       1   Files.Init
        0     0.0%   100.0%       6   Files.ReadChar
        0     0.0%   100.0%       2   Files.WriteInt
        0     0.0%   100.0%       1   Files.WriteLn
        0     0.0%   100.0%       4   Files.WriteString

Total of 554 clock ticks
\endverb
\caption{Execution profile}
\label{f:profile}
\endfig
In this profile, the notation |Fac.%main| refers to the main body
of the |Fac| module; similarly, |Files.%main| refers to the
main body of the |Files| module from the standard library, which is
executed before our program itself starts.

This profile is from too small an example program and too short a run
to draw any firm conclusions, as you can see from the fact that
|Out.Int| was called only twice.  But you can see that a lot of
the time was consumed in the process of reading numbers (procedure
|In.Int|, which also calls |In.IsDigit|).  Since |IsDigit| is such a
simple procedure -- it just tests whether a character is a digit --
perhaps it would be better to do these tests where they are needed,
rather than call a procedure to do them.

The profile data is obtained by counting execution cycles of the
simulated virtual machine, and this is not quite the same thing as
elapsed execution time.  For one thing, the profiler uses an
interpreter for the virtual machine code, rather than the JIT
translator, so the whole program goes much more slowly under profiling
(see the explanation on page~\pageref{p:JIT}).
Another difference is that some
library procedures are actually implemented in C, so no cycles of the
virtual machine are needed to execute them.  You can see the effect
in Figure~\ref{f:profile}, because the procedure |Out.Int| is
implemented as a C primitive, and appears to take much less time than
|In.Int|, which is implemented in Oberon using the primitive~|In.Char|.  
Apart from these primitives, the other operations
of the virtual machine each take a small, fixed time that has the same
order of magnitude for each operation, so that the number of clock
ticks is a good guide to the actual execution time.  Typical
operations, each counted as one clock tick, are fetching the value of
a variable, or adding two numbers together, or comparing two numbers
and jumping if they are equal.  

The \pn{obprof} program also provides a more sophisticated form of
profiling that takes into account the directed graph of calls between
one procedure and another in the program.  In such a {\em call graph
profile}, the time spent in each procedure is also charged to its
callers, so that you can build up a fuller picture of where time is
being spent.  In order to select call graph profiling, you need to
give the \verb/-g/ flag to \pn{obprof}, like this:
\interact
$ _obprof -g prog arg1 ... argn_
\endinter
For further details, see the \pn{obprof} manual page.

It's also possible to get the profiler to count how many times each
line of the program is executed, and then to use an auxiliary program
to produce a listing of the source code where each line is labelled
with its execution count.  To do this, eunt \pn{obprof} with the
command
\interact
$ _obprof -l prog arg1 ... argn_
\endinter
At the end of execution, this will write a file of line counts with
the fixed name \fn{obprof.out}.  To produce an annotated listing, use
the command
\interact
$ _oblist File.m_
\endinter
Separate listings can be made for each module that makes up the program.

Profiling fits in well with a style of programming that begins by
building a simple program that computes the right answers, preferring
simple but perhaps inefficient algorithms and data structures to more
complex ones.  Thus we might use linear search instead of a hash
table, or insertion sort instead of quicksort, just as a way of
getting the program working sooner.  When the program is working
correctly, we can use profiling to identify the places where these
simplified design decisions actually have a significant effect on
performance, and revise just these decisions to make the program
faster.  This method leads to programs that are simple, compact and
reliable whilst still having good performance.  A wise programmer
knows when to stop looking for further improvements: there is no point
working for days to shave a minuscule amount of runtime from a program
that will only be used occasionally.

\chapter Compiler command line.

The compiler command line may list both Oberon source files and
bytecode files.
\description
\item{*.m {\rm or} *.mod\hidewidth}\hfil\break
Source files for input to the compiler.
\item{*.k}
Bytecode object files for linking.
\enddescription
The default behaviour is to compile the source files and link them
together with the object files to form an executable \fn{a.out}.
The compiler's behaviour may be modified with command-line flags.
\description
\item{--c}
Stop the compiling process after producing \fn{.k} files; do not
attempt to produce an executable program.
\item{--o \it file}
Set the output file for linking.
\item{--I \it dir}
Add \fn{dir} to the list of directories to search for imported
modules.
\item{--07}
Accept the Oberon-07 dialect of the language.
\item{--x}
Enable language extensions (see Section~\ref{s:extend}).
\item{--rsb}
Accept a dialect of the language where both language keywords (like
{\vsf begin}) and predefined identifiers (like {\vsf integer}) are
written in lower case.%
\footnote{This flag is provided by special request of Richard Simpson
Bird.}
\item{--O0}
Turn off the peephole optimiser -- mostly used for compiler debugging.
\item{--w}
Turn off compiler warnings about dubious constructions in the source
code.
\item{--b}
Disable runtime checks for null pointers, array bounds, etc.,
sacrificing safety for the sake of screaming speed.
\item{--v}
Print the versions of OBC components, and print the commands that are
used for compiling and linking.
\item{--d \it n}
Set the compiler debugging level to |n|; this adds tracing information
to the normal output of the compiler, but may result in output that
cannot be assembled and linked.
\item{--s}
Strip the symbol table from the output of the linker.  This makes it
impossible to use the profiler or debugger on the resulting
executable.
\item{--j0}
Produce an executable that will run using the bytecode interpreter in
place of the JIT translator.
\item{--k \it n}
Set the runtime stack size to |n| bytes (default 1MB).
\enddescription
The remaining flags are supported on Linux and on Macs with XCode
installed; they are not supported by the normal Windows installation
of OBC.
\description
\item{--C}
Build a specialised version of the runtime system into the executable,
containing only the primitives that are needed, and allowing
additional primitives to be added.
This makes the executable larger, but makes it stand-alone.
\item{*.c}
Files of C code containing primitives.
\item{*.o}
Files of onject code for primitives.
\item{--l \it lib}
(with --C) \ Add the C library \fn{lib} to the runtime system
\item{--L \it dir}
(with --C) \ Add \fn{dir} to the list of directories searched for C
libraries.
\item{--p}
(with --C) \ Link in the profiler in place of the standard runtime
system.
\enddescription

\chapter Language differences and compiler modes.

The Oberon language accepted by the \pn{obc} compiler
is that described in the document
``Oberon--2 Language Definition'', an extension of the document
``Oberon Language Definition'' that appears as
Appendix~A of the book
\quotation
M.~Reiser and N.~Wirth, {\it Programming in Oberon: steps beyond
Pascal and Modula--2}, Addison-Wesley, 1992,
\endquote
with the following exceptions:
\itemize
\item The numeric types are |SHORTINT| (16--bit
integers), |INTEGER| (32--bit integers), |LONGINT| (64--bit integers),
|REAL| (single-precision floating point) and |LONGREAL| (double-precision
floating point).  
The built-in function |ENTIER| returns |INTEGER| instead of |LONGINT|. 

\item Within each module or procedure, it is not necessary to order
the declarations so that (nested) procedure declarations follow all
others.  Instead, declarations of constants, types, variables and
procedures may appear in any order.

\item The scope of a procedure includes the bodies of all procedures
defined at the same nesting level, without the need for `forward
declarations'.
\enditems

\section Oberon07 mode.

The command-line flags \verb/-07/ changes the behaviour of the
compiler so that it accepts a language that is more like Oberon07,
Wirth's 2007 revision of Oberon.
Specifically, the following changes are made:
\enumerate
\item1
Aggregate value parameters are constants passed by reference
\item2
The type |BYTE| and the procedures |FLT|, |FLOOR|, |PACK|, |UNPK|,
|LSL|, |LSR|, |ASR| and |ROR| are provided.
\item3
The procedures |CAP|, |MIN|, |MAX|, |COPY|, |SHORT|, |LONG|, |ASH| and
|ENTIER| are deleted.
\item4
The procedure |SIZE| becomes |SYSTEM.SIZE|.
\item5
The rules for implicit conversion between numeric types are subtly
different.
\item6
Exported variables are impicitly read-only, and `\verb/-/' is not
allowed as an export mark.
\item7
{\sf TRUE} and {\sf FALSE} are keywords, not predefined constants.
\item8
{\sf RETURN} statements are replaced by a {\sf RETURN} clause at the
end of each function.
\item9
{\sf WHILE} statements may have a chain of {\sf ELSIF} parts.
\item{10}
{\sf CASE} statements may discriminate on types.
\endenum
In addition, the following language features are deleted in Oberon07
mode, unless the \verb/-x/ flag is given:
\enumerate
\item{11}
Only pointers to records are allowed, and not to arrays.
\item{12}
No {\sf ELSE} part in {\sf CASE} statements.  The restriction that
{\sf CASE} labels must form a contiguous range is not implemented.
\item{13}
Type-bound procedures are not allowed.
\item{14}
{\sf LOOP} and {\sf EXIT} statements are disallowed.
\endenum

\section Optional language extensions.
\label{s:extend}
The command-line flag \verb/-x/ may be given with or
without the flag \verb/-07/.  In Oberon07 mode, it relaxes some of the
restrictions listed above, and in both modes it enables a number of
language extensions:
\enumerate
\item1 A function may be called as a proper procedure; the result
returned by the function is discarded.
\item2 Enumeration types are supported, with the syntax
\program
TYPE colour = (red, blue, green);
\endprog
\item3 {\sf CASE} statements are allowed for any discrete type
(including enumeration types), not
just |INTEGER| and |CHAR|.
\item4 {\sf FOR} statements are also allowed for any discrete type.
\item5 The function |ORD| may be applied to any discrete type,
not just |CHAR|, and |MIN| and |MAX| also apply to enumeration types.
\item6 The multipart {\sf WHILE} statements and type-based {\sf CASE}
statements of Oberon07 are also enabled in Oberon--2 mode.
\item7 Forward references to types need not be pointer targets: for
example,
\program
TYPE Tree = POINTER TO RECORD left, right: Tree END;
\endprog
is allowed in place of
\program
TYPE Tree = POINTER TO Blob;
  Blob = RECORD left, right: Tree END;
\endprog
\item8 Local procedures may be used as procedural parameters (but may
  not be assigned to procedure variables).
\item9 {\sf CONST} parameters of aggregate types.
\item{10} {\sf ABSTRACT} is added as an additional keyword, allowing
the declaration of abstract records and abstract type-bound procedures.
\endenum
Modules compiled with different language flags may be freely combined
in the same program.

%\input obclib
\chapter OBC Library Reference.

This chapter contains a brief description of the library modules that
are supplied with the \pn{obc} compiler.  The source code to these
modules can be found in the directory \fn{/usr/local/lib/obc}.

\section Module {\it In\/}: Standard input.

This module provides simple input operations on the standard input
channel. 
\spec
\item{PROCEDURE Char(VAR c: CHAR);}
Input one character.
\item{PROCEDURE Int(VAR n: INTEGER);}
Input a decimal integer.
\item{PROCEDURE Real(VAR x: REAL);}
Input a real number.
\item{PROCEDURE Line(VAR s: ARRAY OF CHAR);}
Input one line of text and store it in |s|, removing the terminating
newline character.
\item{VAR Done: BOOLEAN;}
This read-only variable is set to |FALSE| when an input operation
reaches the end of the input file.
\endspec

\section Module {\it Out\/}: Standard output.

This module provides simple output operations on the standard output
channel.
\spec
\item{PROCEDURE Int(n: INTEGER, width: INTEGER);}
Output an integer in decimal, in a field of at least |width| characters.
\item{PROCEDURE Real(x: REAL);}
Output a real number, using scientific notation if it is very large or
very small.
\item{PROCEDURE Fixed(x: REAL; width, dec: INTEGER);}
Output a real number in decimal notation, in a field of at least |width|
characters, with |dec| digits after the decimal point.
\item{PROCEDURE Char(c: CHAR);}
Output a character.
\item{PROCEDURE String(s: ARRAY OF CHAR);}
Output a string.
\item{PROCEDURE Ln;}
Output a newline character.
\endspec

\section Module {\it Err\/}: Standard error.

This module provides simple output operations on the standard error
channel, which remains connected to the user's display even when the
standard output channel is redirected elsewhere.
The interface is identical with that of the module |Out|: thus you can
write |Out.Int(n, w)| to output an integer on the standard output,
and |Err.Int(n, w)| to output it on the standard error channel.

\section Module {\it Files\/}: File input/output.

This module provides I/O operations on named files.
\spec
\item{TYPE File = POINTER TO FileDesc;}
This type represents a file open for reading or writing.
\item{VAR stdin-, stdout-, stderr-: File}
These three read-only variables of type |File| are set to the
standard input, output and error channels.
\item{PROCEDURE Open(name: ARRAY OF CHAR;\\
                mode: ARRAY OF CHAR): File;}
This procedure opens a named file for input, output or both, or
returns |NULL| if the file cannot be opened.  The |mode|
argument takes the same form as the corresponding argument of the C
function |fopen|.  Under MS--DOS, appending \verb/b/ to the mode
causes the file to be opened in binary mode, with no translation of
CR/LF sequences.
\item{PROCEDURE Close(fp: File);}
Close the file |fp|.
\item{PROCEDURE Eof(fp: File): BOOLEAN;}
Test whether the file |fp| is positioned at the end of the file.
\item{PROCEDURE Flush(fp: File);}
Complete any pending I/O operations on file |fp|.
\item{PROCEDURE ReadChar(f: File; VAR c: CHAR);}
Read a single character from file |f|, returning it in the variable |c|.
\item{PROCEDURE WriteInt(f: File; n: INTEGER; w: INTEGER);}
Write the integer |n| to file |f| in decimal using a field of width at
least~|w|.
\item{PROCEDURE WriteReal(f: File; x: REAL);}
Write a real number |x| to file |f|.
\item{PROCEDURE WriteLongReal(f: File; x: LONGREAL);}
Write a double-precsion real |x| to file |f|.
\item{PROCEDURE WriteFixed(f: File; x: LONGREAL;\\
                wid: INTEGER; dec: INTEGER);}
Write real number |x| to file |f| in a field of width |wid|, with
  |dec| digits after the decimal point.
\item{PROCEDURE WriteChar(f: File; c: CHAR);}
Write the character |c| to file |f|.
\item{PROCEDURE WriteString(f: File; s: ARRAY OF CHAR);}
Write the string |s| for file |f|.
\item{PROCEDURE WriteLn(f: File);}
Write a newline character to file |f|.
\item{PROCEDURE Read(f: File; VAR buf: ARRAY OF SYSTEM.BYTE);}
Read characters from file |f| into |buf|.  Note that the Oberon type
rules allow |buf| to be a variable of any desired type.
\item{PROCEDURE Write(f: File; VAR buf: ARRAY OF SYSTEM.BYTE);}
Write characters from |buf| to file |f|.
\item{CONST SeekSet = 0; SeekCur = 1; SeekEnd = 2;}
Constants for use with |Seek|.
\item{PROCEDURE Seek(f: File; offset: INTEGER; whence: INTEGER);}
Position the file |f| at a given offset from the beginning of the file
(if |whence = SeekSet|), from the current position
(if |whence = SeekCur|) or from the end of the
file (if |whence = SeekEnd|).
\item{PROCEDURE Tell(f: File): INTEGER;}
Return the current position of file |f|.
\endspec

\section Module {\it Math\/}: Mathematical functions.

This module contains various mathematical functions on floating-point
numbers.
\spec
\item{PROCEDURE Sqrt(x: REAL): REAL;}
Compute the square root of the argument.
\item{PROCEDURE Sin(x: REAL): REAL;}
The sine function.
\item{PROCEDURE Cos(x: REAL): REAL;}
The cosine function.
\item{PROCEDURE Tan(x: REAL): REAL;}
The tangent function.
\item{PROCEDURE Arctan2(y, x: REAL): REAL;}
The function $"Arctan2"(y, x) = tan^{-1}(y/x)$.  It is defined even when one
of the arguments is zero, and uses the signs of both arguments to
determine the quadrant of the result.  The order of the arguments is
traditional.
\item{PROCEDURE Exp(x: REAL): REAL;}
The exponential function $e^x$.
\item{PROCEDURE Ln(x: REAL): REAL;}
The natural logarithm function.
\item{CONST pi = 3.1415927;}
(Approximately.)
\endspec
All angles are expressed in radians.

\section Module {\it Args\/}: Program arguments.

This module provides access to the command-line arguments given when
the Oberon program was started.

\spec
\item{VAR argc: INTEGER;}
This read-only variable gives the number of arguments, including the
program name.
\item{PROCEDURE GetArg(n: INTEGER; VAR s: ARRAY OF CHAR);}
Copy the |n|'th argument into the string variable |s|.  The arguments
are numbered from 0 to |argc-1|, with the name of the program being
argument~0.
\item{PROCEDURE GetEnv(name: ARRAY OF CHAR; VAR s: ARRAY OF CHAR);}
Copy the value of environment variable |name| into the string
variable~|s|, or set |s| to the empty string if the environment
variable does not exist.
\endspec

\section Module {\it Random\/}: Random numbers.

This module provides a pseudo-random number generator.  Unless the
procedure |Randomize| is called, the sequence of numbers will be the
same on each run of the program.  Actually, this is quite useful,
because it makes the results reproducible.

\spec
\item{PROCEDURE Random(): INTEGER;}
Generate a random integer, between 0 and |MAXRAND| inclusive.
\item{PROCEDURE Roll(n: INTEGER): INTEGER;}
Generate a random integer, uniformly distributed between 0 and $n-1$
inclusive. The result is accurately random only if |n| is fairly small.
\item{PROCEDURE Uniform(): REAL;}
Generate a random real, uniformly distributed on $[0, 1]$.
\item{PROCEDURE Randomize;}
Initialize the random number generator so that it gives a different
sequence of pseudo-random numbers on each run of the program.
\item{CONST MAXRAND = 07FFFFFFFH;}
This constant (equal to $2^{31}-1$) is the largest number that can be
returned by |Random|.
\endspec

\section Module {\it XYplane\/}: Simple bitmap graphics.

Under X windows, this module provides a very simple monochrome
graphics facility.

\spec
\item{CONST W = 640; H = 480;}
These constants give the width and height of the graphics window in
pixels.  The origin is in the bottom left hand corner.
\item{PROCEDURE Open;}
Open the graphics window.
\item{PROCEDURE Clear}
Clear the graphics window to all white.
\item{CONST erase = 0; draw = 1;}
These constants are used as the |mode| parameter of |Dot|.
\item{PROCEDURE Dot(x, y, mode: INTEGER);}
Draw (|mode = draw|) or erase (|mode = erase|) a single pixel
at coordinates $(x, y)$.
\item{PROCEDURE IsDot(x, y: INTEGER): BOOLEAN;}
Test whether a pixel has been drawn at coordinates $(x, y)$.
\item{PROCEDURE Key(): CHAR;}
Test whether a key has been pressed in the graphics window.  If so,
return the character that was typed; otherwise, return the null
character |0X|.
\endspec

The procedure |Key| allows simple keyboard interaction.  It
also handles the events generated by X when the graphics window is
uncovered, so as to fill in the newly-exposed region; this means that
a graphics application should call |Key| in each iteration of its main
loop.

\section Module {\it Conv\/}: Numerical conversions.

This module provides an interface to the procedures for converting
between numbers and strings that are used for input/output.

\spec
\item{PROCEDURE IntVal(s: ARRAY OF CHAR): INTEGER;}
Return the integer value of a string.
\item{PROCEDURE RealVal(s: ARRAY OF CHAR): REAL;}
Return the real value of a string.
\item{PROCEDURE ConvInt(n: INTEGER; VAR s: ARRAY OF CHAR);}
Convert an integer into a decimal string.
\endspec

\section Module {\it String\/}: Operations on strings.

Here is the place where many useful operations on strings will shortly
appear.  Only one is provided so far.
\spec
\item{PROCEDURE Length(s: ARRAY OF CHAR): INTEGER;}
Return the length of |s| up to the first null character.
\endspec

\section Module {\it Bit\/}: Bitwise operations on integers.

This module provides various operations that treat integers as arrays
of 32 bits.

\spec
\item{PROCEDURE And(x, y: INTEGER): INTEGER;}
Bitwise AND: bit |i| of the result is 1 if bit |i| is 1 in both~|x|
and~|y|.
\item{PROCEDURE Or(x, y: INTEGER): INTEGER;}
Bitwise OR: bit |i| of the result is 1 if bit |i| is 1 in either~|x|
or~|y| or both. 
\item{PROCEDURE Xor(x, y: INTEGER): INTEGER;}
Bitwise XOR: bit |i| of the result is 1 if bit |i| is 1 in either~|x|
or~|y|, but not both.
\item{PROCEDURE Not(x: INTEGER): INTEGER;}
Bitwise NOT: bit |i| of the result is 1 if bit~|i| of~|x| is~0.
\endspec

\section Module {\it SYSTEM\/}: Low-level system operations.

This module contains various low-level types and procedures, some of
them non-standard, many of them potentially dangerous, and all of them
non-portable.  In particular, the numeric addresses of items in the
heap may change whenever the garbage collector runs.

\spec
\item{TYPE BYTE}
A formal parameter of type |ARRAY OF SYSTEM.BYTE| can accept an actual
parameter of any type.
\item{TYPE PTR}
A formal parameter of type |SYSTEM.PTR| can accept an actual parameter
of any pointer type.
\item{ADR(v)}
If |v| is any variable then |SYSTEM.ADR(v)| is its address as an
integer.
\item{VAL(T, x)}
If |T| is a type, then |SYSTEM.VAL(T, x)| returns the value~|x|
interpreted as having type~|T|.
\item{PROCEDURE BIT(a: INTEGER, n: INTEGER): BOOLEAN}
The |n|'th bit of the memory word at address~|a|.
\item{PROCEDURE GET(a: INTEGER; VAR v: T)}
The variable |v| can have any basic or pointer type; it is assigned a value
fetched from address~|a|.
\item{PROCEDURE PUT(a: INTEGER; v: T)}
The argument can have any basic or pointer type; the value is stored at
address~|a|.
\item{PROCEDURE MOVE(a1, a1, n: INTEGER)}
A block of |n| bytes at address |a1| is copied to address |a2|.
\endspec

\bye
