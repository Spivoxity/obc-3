/*
 * xmain.c
 * 
 * This file is part of the Oxford Oberon-2 compiler
 * Copyright (c) 2006 J. M. Spivey
 * All rights reserved
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 * 1. Redistributions of source code must retain the above copyright notice,
 *    this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright notice,
 *    this list of conditions and the following disclaimer in the documentation
 *    and/or other materials provided with the distribution.
 * 3. The name of the author may not be used to endorse or promote products
 *    derived from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 * OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
 * IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS;
 * OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
 * WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
 * OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
 * ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

#define EXTERN
#include "obx.h"
#include "keiko.h"
#include "exec.h"

const char *copyright = "Copyright (C) 1999 J. M. Spivey";

extern int vm_debug;

/* Helper functions for the loader */

module make_module(char *name, uchar *addr, int chksum, int nlines) {
     module m = (module) scratch_alloc(sizeof(struct _module), FALSE);
     m->m_name = name;
     m->m_addr = addr;
#ifdef PROFILE
     m->m_nlines = nlines;
     m->m_lcount = NULL;
     if (lflag && nlines > 0) {
	  m->m_lcount = 
	       (unsigned *) scratch_alloc(nlines * sizeof(unsigned), TRUE);
	  memset(m->m_lcount, 0, nlines * sizeof(int));
     }
#endif
#ifdef OBXDEB
     debug_message("module %s %#x", name, chksum);
#endif
     return m;
}

proc make_proc(char *name, uchar *addr) {
     proc p = (proc) scratch_alloc(sizeof(struct _proc), FALSE);
     p->p_name = name;
     p->p_addr = (value *) addr;
#ifdef PROFILE
     p->p_calls = p->p_rec = p->p_self = p->p_child = 0;
     p->p_parents = p->p_children = NULL;
#endif
#ifdef OBXDEB
     debug_message("proc %s %#x %#x %d", name, (unsigned) addr, 
		   (unsigned) p->p_addr[CP_CODE].x, p->p_addr[CP_SIZE].i);
#endif
     return p;
}

void make_symbol(const char *kind, char *name, uchar *addr) {
#ifdef OBXDEB
     debug_message("%s %s %#x", kind, name, (unsigned) addr);
#endif
}

/* Runtime errors */

#ifndef OBXDEB
#define TOP 5			/* Number of frames shown at top and bottom */
#define BOT 5
#define GAP 10			/* Don't omit less than this many frames */
#define NBUF (BOT+GAP-1)

static void backtrace(value *bp) {
     value *fp = bp, *cp = bp[CP].p;
     proc p = find_proc(cp);
     int n, j;
     proc fbuf[NBUF];
     
     fprintf(stderr, "In procedure %s\n", p->p_name);

     /* Chain down the stack, printing the first TOP frames,
	and saving the last NBUF in a circular buffer. */
     for (n = 0; ; n++) {
	  /* Each frame contains the cp and bp of its caller */
	  fp = fp[BP].p;	/* Base pointer of next frame */
	  if (fp == NULL) break;
	  cp = fp[CP].p;	/* Constant pool of next frame */
	  fbuf[n%NBUF] = p = find_proc(cp);
	  if (n < TOP)
	       fprintf(stderr, "   called from %s\n", p->p_name);
     }

     /* Now the last NBUF frames are f(n-NBUF), ..., f(n-1)
	where f(i) = fbuf[i%NBUF] -- unless there are fewer
	then NBUF frames in all. */

     if (n < TOP+GAP+BOT) 
	  /* Print the n-TOP frames not printed already */
	  j = TOP;
     else {
	  /* Omit n-(TOP+BOT) frames (at least GAP) and print the 
	     last BOT frames */
	  fprintf(stderr, "   ... %d intervening frames omitted ...\n", 
		  n-(TOP+BOT));
	  j = n-BOT;
     }

     /* Print frames j, ..., n-1 */
     for (; j < n; j++)
	  fprintf(stderr, "   called from %s\n", fbuf[j%NBUF]->p_name);
}
#endif

static const char *message(int code) {
     switch (code) {
     case E_CAST:
	  return "dynamic type error in cast";
     case E_ASSIGN:
	  return "dynamic type error in record assignment";
     case E_CASE:
	  return "no matching label in CASE statement";
     case E_WITH:
	  return "no matching type guard in WITH statement";
     case E_ASSERT:
	  return "assertion failed (%d)";
     case E_RETURN:
	  return "function failed to return a result";
     case E_BOUND:
	  return "array bound error";
     case E_NULL:
	  return "null pointer error";
     case E_DIV:
	  return "DIV or MOD by zero";
     case E_FDIV:
	  return "division by zero";
     case E_STACK:
	  return "stack overflow";
     case E_GLOB:
	  return "assignment of local procedure";
     default:
	  return "the impossible has happened";
     }
}

/* error_stop -- runtime error with explicit message text */
void error_stop(const char *msg, int line, value *bp, uchar *pc) {
     value *cp = bp[CP].p;

#ifdef OBXDEB
     char buf[256];
     sprintf(buf, msg, ob_res.i);
     debug_break(cp, bp, pc, "error %d %s", line, buf);
#else
     module mod = find_module(cp);

     fprintf(stderr, "Runtime error: ");
     fprintf(stderr, msg, ob_res.i);
     if (line > 0) fprintf(stderr, " on line %d", line);
     if (mod != NULL && strcmp(mod->m_name, "_Builtin") != 0) 
	  fprintf(stderr, " in module %s", mod->m_name);
     fprintf(stderr, "\n");
     fflush(stderr);

     if (nprocs == 0)
	  fprintf(stderr, "(No debugging information available)\n");
     else if (bp != NULL)
	  backtrace(bp);

     fflush(stderr);
#endif

     exit(2);
}

/* runtime_error -- report a runtime error */
void runtime_error(int m, int line, value *bp, uchar *pc) {
     error_stop(message(m), line, bp, pc);
}

/* rterror -- simple version of runtime_error for JIT */
void rterror(int num, int line, value *bp) {
     runtime_error(num, line, bp, NULL);
}

#ifdef JIT
/* jit_trap -- translate procedure on first call */
void jit_trap(value *bp) {
     value *cp = bp[CP].p;
     jit_compile(cp);
     cp[CP_PRIM].z(bp);
}

/* interp -- dummy interpreter */
void interp(value *bp) {
     panic("no interpreter");
}
#endif


/* Startup */

static void run(value *prog) {
     value *sp;

     /* Allow 32-word safety margin (for tracing) */
     sp = (value *) (stack + stack_size) - 32; 

     sp -= HEAD; 
     sp[BP].p = NULL; 
     sp[PC].x = NULL; 
     sp[CP].p = prog;
     (*(prog[CP_PRIM].z))(sp);
}

boolean custom_file(char *name) {
     char buf[4];
     FILE *fp;
     int nread;
     boolean result;

     fp = fopen(name, "rb");
     if (fp == NULL) return FALSE;
     fseek(fp, -sizeof(trailer), SEEK_END);
     nread = fread(buf, 1, 4, fp);
     if (nread < 4 || strncmp(buf, MAGIC, 4) != 0)
	  result = FALSE;
     else {
	  fseek(fp, 0, SEEK_SET);
	  nread = fread(buf, 1, 2, fp);
	  result = (nread == 2 && strncmp(buf, "#!", 2) != 0);
     }
     fclose(fp);
     return result;
}
 
#ifdef WINDOWS
#include <windows.h>
#include <winbase.h>

char *search_path(char *name) {
     static char buf[_MAX_PATH];
     char *filepart;

     if (SearchPath(NULL, name, ".exe", _MAX_PATH, buf, &filepart) == 0)
	  return NULL;

     return buf;
}
#else
#include <sys/stat.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

char *search_path(char *name) {
     char *path, *p, *q, *r;
     static char buf[256];
     struct stat stbuf;

     if (name == NULL || strchr(name, '/') != NULL) return name;

     path = getenv("PATH");
     if (path == NULL) return NULL;

     for (p = path; p != NULL; p = q) {
	  q = strchr(p, ':');
	  if (q == NULL) {
	       strcpy(buf, p);
	       r = buf + strlen(p);
	  } else {
	       strncpy(buf, p, q-p);
	       r = buf + (q-p); q++;
	  }
	  if (r > buf) *r++ = '/';
	  strcpy(r, name);

	  if (access(buf, R_OK) == 0 && stat(buf, &stbuf) == 0
	      && S_ISREG(stbuf.st_mode))
	       return buf;
     }

     return NULL;
}
#endif

#ifdef PROFILE
#define MYNAME "profiler"
#else
#ifdef OBXDEB
#define MYNAME "debugging monitor"
#else
#define MYNAME "runtime system"
#endif
#endif

#define argc saved_argc
#define argv saved_argv

static char *progname;
#ifdef PROFILE
static char *profout;
static const char *dumpname = "obprof.out";
#endif

static void usage(void) {
#ifdef PROFILE
     fprintf(stderr, 
	     "Usage: %s [-g] [-pl] [-o file] program [arg ...]\n", 
	     progname);
#else
     fprintf(stderr, "Usage: %s program [arg ...]\n", progname);
#endif
     fflush(stderr);
     _exit(1);
}

/* read_flags -- interpret flags */
static void read_flags(void) {
     for (;;) {
	  argc--; argv++;
	  if (argc == 0 || argv[0][0] != '-') return;

	  if (strcmp(argv[0], "--") == 0) {
	       argc--; argv++;
	       return;
	  } else if (strcmp(argv[0], "-d") == 0) {
	       dflag++;
	  } else if (strcmp(argv[0], "-v") == 0) {
	       fprintf(stderr, "Oxford Oberon-2 %s version %s%s%s\n",
		       MYNAME, PACKAGE_VERSION,
#ifdef JIT
		       " (JIT)",
#else
		       "",
#endif
#ifdef DEBUG
		       " (debug)"
#else
		       ""
#endif
		    );
	       exit(0);
	  }
#ifdef PROFILE
	  else if (argc >= 2 && strcmp(argv[0], "-o") == 0) {
	       profout = argv[1];	
	       argc--; argv++;
	  } else if (strcmp(argv[0], "-g") == 0) {
	       gflag = TRUE;
	  } else if (strcmp(argv[0], "-l") == 0 
		     || strcmp(argv[0], "-pl") == 0) {
	       lflag = TRUE;
	  }
#endif
#ifdef TRACE
	  else if (strcmp(argv[0], "-q") == 0) {
	       qflag++;
	  }
#endif
#ifdef OBXDEB
	  else if (argc >= 2 && strcmp(argv[0], "-p") == 0) {
	       debug_socket = argv[1];
	       argc--; argv++;
	  }
#endif
	  else {
	       usage();
	  }
     }
}

#ifdef PROFILE
static void dump_lcounts(void) {
     int m, n;
     FILE *fp;

     fp = fopen(dumpname, "w");
     if (fp == NULL) {
	  fprintf(stderr, "%s: cannot write\n", dumpname);
	  exit(1);
     }

     for (m = 0; m < nmods; m++)
	  for (n = 1; n <= modtab[m]->m_nlines; n++)
	       if (modtab[m]->m_lcount[n-1] > 0)
		    fprintf(fp, "%s %d %u\n", modtab[m]->m_name, n, 
			    modtab[m]->m_lcount[n-1]);

     fclose(fp);
}

static void print_profile(void) {
     FILE *fp = stderr;
     int i;

     if (profout != NULL) {
	  fp = fopen(profout, "w");
	  if (fp == NULL) {
	       fprintf(stderr, "%s: cannot write\n", profout);
	       exit(1);
	  }

	  fprintf(fp, "Command line:\n\n");
	  fprintf(fp, "  %s", saved_argv[0]);
	  for (i = 1; i < saved_argc; i++)
	       fprintf(fp, " %s", saved_argv[i]);
	  fprintf(fp, "\n\n");
     }

     profile(fp);

     if (fp != stderr) fclose(fp);
}
#endif

/* xmain_exit -- exit after program has finished */
void NORETURN xmain_exit(int status) {
#ifdef OBXDEB
     debug_break(NULL, NULL, NULL, "exit");
#endif
#ifdef PROFILE
     print_profile();
     if (lflag) dump_lcounts();
#endif
     exit(status);
}

/* error_exit -- exit after fatal error */
void NORETURN error_exit(int status) {
#ifdef OBXDEB
     debug_message("quit");
#endif
     exit(status);
}

/* The interpreter can be invoked in three ways:
   (i)   Explicitly as "obx [flags] bytefile args"

   (ii)  Via a #! script as "obx bytefile args"
         or "bytefile bytefile args" under some Unixes

   (iii) In a glued-together executable as "bytefile args"

   Following the example of CAML Light, we recognize (iii) by seeing
   if argv[0] names a bytefile that does not begin with #!.  In that
   case, we read that file for the bytecodes, and the program's args
   follow immediately; otherwise, we look for flags and the name of
   the bytefile before the program's args.  In either case, we must be
   prepared to search the shell path to find the bytefile. 

   These rules are modified a bit if a custom file is built for
   profiling: in that case, we look for switches even in case (iii). */

int main(int ac, char *av[]) {
     FILE *fp;
     char *codefile;

     argc = ac; argv = av;
     progname = argv[0];

     /* Read the command line first to handle -v */
     codefile = search_path(argv[0]);
     if (codefile != NULL && custom_file(codefile)) {
#ifdef PROFILE
	  char *prog = argv[0];
	  read_flags();
	  /* Fill the program name back in as argv[0] */
	  argc++; argv--;
	  argv[0] = prog;
#endif
     } else {
	  read_flags();
	  if (argc < 1) usage();
	  codefile = search_path(argv[0]);     
     }

#ifdef OBXDEB
     /* Now connect to the debugger process */
     debug_init();
#endif

     if (codefile == NULL) panic("can't find %s", argv[0]);

     gc_init();

#ifdef JIT
     primtab[0] = jit_trap;
     vm_debug = (dflag > 1);
#endif

     fp = fopen(codefile, "rb");
     if (fp == NULL) panic("can't open %s", codefile);
     load_file(fp);
     fclose(fp);

#ifdef TRACE
     if (dflag) dump();
     if (qflag) exit(0);
#endif

#ifdef PROFILE
     if (nprocs == 0) 
	  panic("no symbol table in object file");

     prof_init();
#endif	  

#ifdef OBXDEB
     debug_break(NULL, NULL, NULL, "ready");
#endif
#ifdef DEBUG
     if (dflag)
	  printf("Starting program at address %d\n", ((uchar *) entry) - dmem);
#endif
     run(entry);
     xmain_exit(0);
}

#ifdef WINDOWS
#ifdef OBXDEB
#define OBGETC 1
#endif
#endif

/* obgetc -- version of getc that compensates for Windows quirks */
int obgetc(FILE *fp) {
#ifdef OBGETC
     /* Even if Ctrl-C is trapped, it causes a getc() call on the console
	to return EOF. */
     for (;;) {
	  int c = getc(fp);
	  if (c == EOF && intflag && prim_bp != NULL) {
	       value *cp = prim_bp[CP].p;
	       debug_break(cp , prim_bp, NULL, "interrupt");
	       continue;
	  }
	  return c;
     }
#else
     return getc(fp);
#endif
}
