/*
 * oblink.c
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
#include "oblink.h"
#include "keiko.h"

const char *copyright = "Copyright (C) 1999 J. M. Spivey";

/* The module table has one entry for each module that appears in the
   input files.  There's another table kept by the linker itself that
   has one entry for each module actually selected for linking. */

struct _module {
     char *m_file;		/* Name of the file */
     char *m_name;		/* Name of the module */
     boolean m_lib, m_needed;	/* Whether a library module, whether needed */
     int m_dep;			/* Index of first prerequisite */
     int m_check;		/* Checksum */
};

static growdecl(module);
#define module growbuf(module, struct _module)
#define nmodules growsize(module)

/* The imports of module m are dep[module[m].m_dep .. module[m+1].m_dep) */

static growdecl(dep);
#define dep growbuf(dep, int)
#define ndeps growsize(dep)

#ifdef HAVE_GETOPT_LONG_ONLY
static int nfiles;
static char **file;
#else
static growdecl(file);
#define file growbuf(file, char *)
#define nfiles growsize(file)
#endif

#define MAXLINE 1024

static char line[MAXLINE];
static int nwords;
static char *words[MAXWORDS];

static boolean stdlib = TRUE;
static char *lscript = (char *) "lscript";
static char *interp = NULL;
static char *outname = (char *) "a.out";
static char *libdir = NULL;
static char *rtlibdir = NULL;

static int find_module(char *name) {
     int i;

     for (i = 0; i < nmodules; i++)
	  if (strcmp(name, module[i].m_name) == 0)
	       return i;

     return -1;
}

/* scan -- scan a file for MODULE and IMPORT directives */
static void scan(char *name, boolean islib)  {
     FILE *fp;
     int m = -1, m2, chksum;

     err_file = must_strdup(name);
     fp = fopen(name, "r");
     if (fp == NULL) {
	  perror(name);
	  exit(2);
     }

     while (fgets(line, MAXLINE, fp) != NULL) {
	  nwords = split_line(line, words);
	  if (nwords == 0) continue;

	  if (strcmp(words[0], "MODULE") == 0) {
	       char *mname = words[1];
	       m = find_module(mname);
	       if (m >= 0) {
		    if (module[m].m_lib)
			 error("%s has the same name as a library module", 
			       words[1]);
		    else 
			 error("%s is loaded more than once", words[1]);
	       }
	       
	       buf_grow(module);
	       m = nmodules;
	       module[m].m_file = name;
	       module[m].m_name = must_strdup(mname);
	       module[m].m_lib = islib;
	       module[m].m_needed = FALSE;
	       module[m].m_dep = ndeps;
	       module[m].m_check = strtoul(words[2], NULL, 0);
	       nmodules++;
	  } else if (strcmp(words[0], "IMPORT") == 0) {
	       if (m < 0)
		    error("IMPORT appears before MODULE in %s", name);

	       m2 = find_module(words[1]);
	       chksum = strtoul(words[2], NULL, 0);
	       buf_grow(dep);
	       if (m2 < 0) 
		    error("%s imports %s -- please load it first",
			  module[m].m_name, words[1]);
	       else {
		    dep[ndeps++] = m2;
		    if (module[m2].m_check != chksum)
			 error("checksum of module %s does not match value"
			       " expected by module %s", 
			       words[1], module[m].m_name);
	       }
	  } else if (strcmp(words[0], "PRIM") == 0) {
	       if (islib && !custom
#ifdef DYNLINK
		   && *words[1] != '*'
#endif
		    ) make_prim(words[1]);
	  } else if (strcmp(words[0], "ENDHDR") == 0) {
	       break;
	  } else {
	       panic("*bad directive %s in file header", words[0]);
	  }
     }

     fclose(fp);
}			       

static void scan_files(void) {
     FILE *fp;
     int i;
     char buf[128];

     if (stdlib) {
	  sprintf(buf, "%s%s%s", libdir, PATHSEP, lscript);
	  fp = fopen(buf, "r");
	  if (fp == NULL) {
	       perror(buf);
	       exit(2);
	  }

	  while (fgets(line, MAXLINE, fp) != NULL) {
	       line[strlen(line)-1] = '\0';
	       sprintf(buf, "%s%s%s", libdir, PATHSEP, line);
	       scan(must_strdup(buf), TRUE);
	  }

	  fclose(fp);
     }

     for (i = 0; i < nfiles; i++)
	  scan(file[i], FALSE);
}

/* load_needed -- load files containing needed modules */
static void load_needed() {
     FILE *fp;
     int i;
     char *name;

     for (i = 0; i < nmodules; i++) {
	  if (!module[i].m_needed) continue;

	  name = module[i].m_file;
	  err_file = name;
	  fp = fopen(name, "r");
	  if (fp == NULL) {
	       perror(name);
	       exit(2);
	  }

	  while (fgets(line, MAXLINE, fp) != NULL) {
	       nwords = split_line(line, words);
	       if (nwords == 0) continue;
	       put_inst(words[0], &words[1], nwords-1);
	  }

	  fclose(fp);
     }
}

/* trace_imports -- compute needed modules */
static void trace_imports(void) {
     int i, j;

     for (i = nmodules-1; i >= 0; i--) {
	  if (!module[i].m_lib || strcmp(module[i].m_name, "_Builtin") == 0) 
	       module[i].m_needed = TRUE;

	  if (module[i].m_needed)
	       for (j = module[i].m_dep; j < module[i+1].m_dep; j++)
		    module[dep[j]].m_needed = TRUE;
     }

#ifdef DEBUG
     if (dflag) {
	  fprintf(stderr, "Needed:");
	  for (i = 0; i < nmodules; i++)
	       if (module[i].m_needed)
		    fprintf(stderr, " %s", module[i].m_name);
	  fprintf(stderr, "\n");
     }
#endif
}

/* gen_main -- generate the main program */
static void gen_main(void) {
     int i;
     char buf[128];

     if (known("MAIN")) return;

     err_file = (char *) "main program";

     /* For completeness, generate a header listing all loaded modules. */
     gen_inst("MODULE %%Main 0 0");
     for (i = 0; i < nmodules; i++) {
	  if (strcmp(module[i].m_name, "_Builtin") == 0 
	      || !module[i].m_needed) continue;
	  gen_inst("IMPORT %s %#x", module[i].m_name, module[i].m_check);
     }
     gen_inst("ENDHDR");

     gen_inst("PROC MAIN 0 4 0");
     /* Code to call each module body */
     for (i = 0; i < nmodules; i++) {
	  if (!module[i].m_needed) continue;
	  sprintf(buf, "%s.%%main", module[i].m_name);
	  if (known(buf)) {
	       gen_inst("GLOBAL %s", buf);
	       gen_inst("CALL 0");
	  }
     }
     gen_inst("RETURN");
     gen_inst("END");

     /* Make global pointer map */
     gen_inst("DEFINE GCMAP");
     for (i = 0; i < nmodules; i++) {
	  if (!module[i].m_needed) continue;
	  sprintf(buf, "%s.%%gcmap", module[i].m_name);
	  if (known(buf)) {
	       gen_inst("WORD GC_MAP");
	       gen_inst("WORD %s", buf);
	  }
     }
     gen_inst("WORD GC_END");
}

#ifdef HAVE_GETOPT_LONG_ONLY
#include <getopt.h>

#define SCRIPT 1

static struct option longopts[] = {
     { "script", required_argument, NULL, SCRIPT },
     { "custom", no_argument, &custom, TRUE },
     { "dump", no_argument, &dump, TRUE },
     { "nostdlib", no_argument, &stdlib, FALSE },
     { "pl", no_argument, &linecount, TRUE },
     { NULL, 0, NULL, 0 }
};

/* get_options -- analyse arguments */
static void get_options(int argc, char **argv) {
     for (;;) {
	  int c = getopt_long_only(argc, argv, "dvsgmi:L:R:o:k:", 
				   longopts, NULL);

	  if (c == -1) break;

	  switch (c) {
	  case 'd':
	       dflag++; break;
	  case 'v':
	       printf("Oxford Oberon-2 linker version %s\n", PACKAGE_VERSION);
	       exit(0);
	       break;
	  case 's':
	       sflag = TRUE; break;
	  case 'g':
	       gflag = TRUE; break;
	  case 'i':
	       interp = optarg; break;
	  case 'L':
	       libdir = optarg; break;
	  case 'R':
	       rtlibdir = optarg; break;
	  case 'o':
	       outname = optarg; break;
	  case 'k':
	       stack_size = atoi(optarg);
	       if (stack_size < MIN_STACK) stack_size = MIN_STACK;
	       break;
	  case 0:
	       /* Long option with flag */
	       break;
	  case SCRIPT:
	       /* -script */
	       lscript = optarg; break;
	  case '?':
	       /* Error has been reported by getopt */
	       exit(2);
	       break;
	  default:
	       panic("*bad option");
	  }
     }

     nfiles = argc - optind;
     file = &argv[optind];
}

#else

/* get_options -- analyse arguments */
static void get_options(int argc, char **argv) {
     int i;

     buf_init(file, INIT_MODS, 1, char *, "files");

     for (i = 1; i < argc; i++) {
	  if (strcmp(argv[i], "-d") == 0)
	       dflag++;
	  else if (strcmp(argv[i], "-v") == 0) {
	       printf("Oxford Oberon-2 linker version %s\n", PACKAGE_VERSION);
	       exit(0);
	  } else if (strcmp(argv[i], "-s") == 0) {
	       sflag = TRUE;
	  } else if (strcmp(argv[i], "-g") == 0) {
	       gflag = TRUE;
	  } else if (strcmp(argv[i], "-script") == 0) {
	       if (++i == argc) panic("missing argument after -script");
	       lscript = argv[i];
	  } else if (strcmp(argv[i], "-custom") == 0) {
	       custom = TRUE;
	  } else if (strcmp(argv[i], "-dump") == 0) {
	       dump = TRUE;
	  } else if (strcmp(argv[i], "-nostdlib") == 0) {
	       stdlib = FALSE;
	  } else if (strcmp(argv[i], "-pl") == 0) {
	       linecount = TRUE;
	  } else if (strcmp(argv[i], "-i") == 0) {
	       if (++i == argc) panic("missing argument after -i");
	       interp = argv[i];
	  } else if (strcmp(argv[i], "-L") == 0) {
	       if (++i == argc) panic("missing argument after -L");
	       libdir = argv[i];
	  } else if (strcmp(argv[i], "-R") == 0) {
	       if (++i == argc) panic("missing argument after -R");
	       rtlibdir = argv[i];
	  } else if (strcmp(argv[i], "-o") == 0) {
	       if (++i == argc) panic("missing argument after -o");
	       outname = argv[i];
	  } else if (strcmp(argv[i], "-k") == 0) {
	       if (++i == argc) panic("missing argument after -k");
	       stack_size = atoi(argv[i]);
	       if (stack_size < MIN_STACK) stack_size = MIN_STACK;
	  } else if (argv[i][0] == '-') {
	       panic("unknown switch %s", argv[i]);
	  } else {
	       buf_grow(file);
	       file[nfiles] = argv[i];
	       nfiles++;
	  }
     }
}
#endif

int main(int argc, char **argv) {
     progname = argv[0];

     buf_init(module, INIT_MODS, 1, struct _module, "modules");
     buf_init(dep, INIT_MODS, 1, int, "dependencies");

     stack_size = STACK_SIZE;

     get_options(argc, argv);
     if (nfiles == 0 && !dump) panic("no input files");
     if (stdlib && libdir == NULL) panic("no libdir specified");
     if (rtlibdir == NULL) rtlibdir = libdir;

     make_prim("INTERP");
     make_prim("DLTRAP");
     
#define bind(x) def_global(find_symbol(#x), ABS, x, X_SYM)

     bind(GC_BASE); bind(GC_REPEAT); bind(GC_BLOCK);
     bind(GC_MAP); bind(GC_FLEX); bind(GC_END);
     bind(E_CAST); bind(E_ASSIGN); bind(E_CASE);
     bind(E_WITH); bind(E_ASSERT); bind(E_RETURN);
     bind(E_BOUND); bind(E_NULL); bind(E_DIV);
     bind(E_FDIV); bind(E_STACK); bind(E_GLOB);

     /* First pass -- check for dependencies */
     scan_files();

     /* Compute needed modules */
     buf_grow(module);
     module[nmodules].m_dep = ndeps;
     trace_imports();

     if (status != 0) return status;

     /* Second pass -- link the modules that are needed */
     if (!dump) {
	  init_linker(outname, interp);
	  load_needed();
	  gen_main();
	  if (rtlibdir != NULL)
	       save_string("LIBDIR", rtlibdir);
	  end_linking();
     }

     if (dump || custom) {
	  printf("/* Primitive table -- generated by oblink */\n\n");
	  printf("#include \"obx.h\"\n\n");
	  dump_prims(stdout);
     }

     return status;
}
