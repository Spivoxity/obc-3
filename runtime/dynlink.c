/*
 * loader.c
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

/*
Initially, the procedure descriptor for each such primitive has the
trap handler |dltrap| as its interpreter, and the CP_CODE field of
the descriptor points to the name of the primitive as a string.
When the primitive is first called, the |dltrap| primitive looks up
the symbol and binds the primitive to its value for future use.
Finally, it calls the newly-loaded primitive to complete the first
call.

Function |load_lib| loads a dynamic library.  Each Oberon module that
links to a dynamic library should call |DynLink.Load("path")| 
in its initialization part.
*/

#include "obx.h"

#ifdef DYNLINK
#ifndef __USE_GNU
#define __USE_GNU
#endif
#include <dlfcn.h>
#endif

void load_lib(char *fname) {
#ifdef DYNLINK
     char buf[128];
	
     /* If the library name contains no slash, look in the OBC lib directory 
        and append the extension ".so" or ".dylib" au chois */
     if (strchr(fname, '/') == NULL) {
	  char *dir = getenv("OBC_LIB");
	  if (dir == NULL) dir = libpath;
	  if (dir == NULL) panic("no runtime library");
	  strcpy(buf, dir);
	  strcat(buf, "/");
	  strcat(buf, fname);
	  strcat(buf, DLEXT);
	  fname = buf;
     }

     /* Load the library */
     if (dlopen(fname, RTLD_LAZY|RTLD_GLOBAL) == NULL) 
	  panic("Can't find library %s: %s", fname, dlerror());
#endif
}

#ifndef DYNLINK
primitive *find_prim(char *name) {
     int i;

     for (i = 0; primtab[i].p_name != NULL; i++) {
	  if (strcmp(name, primtab[i].p_name) == 0)
	       return primtab[i].p_prim;
     }

     return NULL;
}
#endif

void dltrap(value *sp) {
     value *bp = sp;
     value *cp = valptr(bp[CP]);
     char *name = (char * ) pointer(cp[CP_CODE]);
     primitive *prim;

#ifdef DYNLINK
     prim = (primitive * ) dlsym(RTLD_DEFAULT, name);
#else
     prim = find_prim(name);
#endif

     if (prim == NULL) 
	  panic("couldn't find primitive %s", name);

     /* Bind the primitive to the library symbol */
     cp[CP_PRIM].a = wrap_prim(prim);

     /* Call the primitive for the first time */
     ( * prim)(sp);
}

