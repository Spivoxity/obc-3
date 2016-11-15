/*
 * obdb.c
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

#include "config.h"
#include "wrap.h"
#include <windows.h>

#define DEBUGGER "obdb1.exe"
#define MONITOR "obxdeb.exe"

void usage(void)
{
     fprintf(stderr, "Usage: obdb command args ...\n");
     exit(2);
}

int dflag = 0;

int startup(int argc, char **argv)
{
     int i, status;
     char cmdbuf[MAX];

     sprintf(cmdbuf, "%s\\%s", obclib, DEBUGGER);
     arg(cmdbuf); 
     if (dflag) arg("-d");
     arg("-I"); arg(obclib); 
     arg("-i"); argf("%s\\%s", obclib, MONITOR);
     arg("-R"); arg(obclib);
     for (i = 0; i < argc; i++) arg(argv[i]);
     status = command(cmdbuf, 0);
     return status;
}

void versions(void)
{
     char argbuf[MAX];

     printf("Oxford Oberon-2 debugger driver version %s [build %s]\n", 
	    PACKAGE_VERSION, REVID);
     sprintf(argbuf, "%s\\%s", obclib, DEBUGGER);
     arg(argbuf); arg("-v"); command(argbuf, 0);
     sprintf(argbuf, "%s\\%s", obclib, MONITOR);
     arg(argbuf); arg("-v"); command(argbuf, 0);
     exit(0);
}

BOOL WINAPI ctrl_handler(DWORD event) {
     if (event != CTRL_C_EVENT && event != CTRL_BREAK_EVENT) return FALSE;
     return TRUE;
}

int main(int argc, char *argv[])
{
     int i, status = 0;

     check_path(DEBUGGER);

     for (i = 1; i < argc; i++) {
	  char *s = argv[i];

#define m(str) (strcmp(s, str) == 0)

	  if (m("-v"))
	       versions();
	  else if (m("-d"))
	       dflag = 1;
	  else if (s[0] == '-')
	       usage();
	  else 
	       break;
     }

     if (argc - i < 1) usage();

     SetConsoleCtrlHandler(ctrl_handler, TRUE);
     status = startup(argc - i, argv + i);
     return status;
}
