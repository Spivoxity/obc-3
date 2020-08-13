/*
 * wrap.c
 * 
 * This file is part of the Oxford Oberon-2 compiler
 * Copyright (c) 2006--2016 J. M. Spivey
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

#include "wrap.h"
#include <unistd.h>
#include <stdarg.h>
#include <windows.h>

static char cmd[MAX];
static char argspace[ARGSPACE];
static char *args[MAXARGS];
int nargs = 0;
char *argptr = argspace;

void arg(char *s) {
     args[nargs++] = argptr;
     if (strchr(s, ' ') == NULL)
	  strcpy(argptr, s);
     else {
	  strcpy(argptr, "\"");
	  strcat(argptr, s);
	  strcat(argptr, "\"");
     }
     argptr += strlen(argptr)+1;
}

void command(char *fmt, ...) {
     va_list va;
     va_start(va, fmt);
     vsprintf(cmd, fmt, va);
     va_end(va);

     nargs = 0;
     arg(cmd);
}

void argf(char *fmt, ...) {
     char buf[MAX];
     va_list va;
     va_start(va, fmt);
     vsprintf(buf, fmt, va);
     va_end(va);
     arg(buf);
}

int launch(int verbose) {
     int i, status;

     if (verbose) {
	  fprintf(stderr, "Running %s", cmd);
	  for (i = 1; i < nargs; i++)
	       fprintf(stderr, " %s", args[i]);
	  fprintf(stderr, "\n");
     }

     fflush(stderr);

     args[nargs] = NULL;
     status = spawnvp(P_WAIT, cmd, (char * const *) args);
     if (verbose && status != 0) 
          fprintf(stderr, "*** Command returned status %d\n", status);

     nargs = 0; argptr = argspace;

     return status;
}

int redirect(int verbose, char *outfile)
{
     int old_stdout, new_stdout, status;

     new_stdout = open(outfile, O_WRONLY|O_CREAT|O_TRUNC, S_IREAD|S_IWRITE);
     if (new_stdout < 0) return 2;
     old_stdout = dup(1);
     dup2(new_stdout, 1);
     close(new_stdout);

     status = launch(verbose);

     dup2(old_stdout, 1);
     close(old_stdout);
     return status;
}

char *obcdir;

void find_path(void) {
     static char exedir[MAX];
     char *p; int count;

     /* Trust the environment variable */
     obcdir = getenv(ENVNAME);

     /* Try the parent of the directory containing the executable */
     if (obcdir == NULL) {
	  GetModuleFileName(NULL, exedir, MAX);
	  p = exedir + strlen(exedir);
	  count = 0;
	  while (p > exedir && count < 2)
	       if (*--p == '\\') count++;
	  if (p > exedir) {
	       *p = '\0';
	       obcdir = exedir;
	  }
     }


     if (obcdir == NULL) {
	  fprintf(stderr, "Can't find the OBC directory\n");
	  exit(2);
     }
}
