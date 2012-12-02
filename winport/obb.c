/*
 * obb.c
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

#include "wrap.h"

#define BROWSER "obb1.exe"

void usage(void)
{
     fprintf(stderr,
	     "Usage: obb module\n");
     exit(2);
}

int browse(char *module)
{
     int status;
     char cmdbuf[MAX];

     sprintf(cmdbuf, "%s\\%s", obclib, BROWSER);
     arg(cmdbuf); arg("-I"); arg(obclib); arg(module);
     status = command(cmdbuf, 0);
     return status;
}

void versions(void)
{
     char argbuf[MAX];

     printf("Oxford Oberon-2 browser driver version %s\n", PACKAGE_VERSION);

     sprintf(argbuf, "%s\\%s", obclib, BROWSER);
     arg(argbuf); arg("-v");
     command(argbuf, 0);
}

int main(int argc, char *argv[])
{
     int status = 0;

     if (argc != 2) usage();

     check_path(BROWSER);

     if (strcmp(argv[1], "-v") == 0) {
	  versions();
	  exit(0);
     }

     status = browse(argv[1]);

     return status;
}
