(*
 * DynLink.m
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
 *)

MODULE DynLink;

(* This module allows collections of primitives to be loaded as
   dynamic libraries.

   Initially, the procedure descriptor for each such primitive has the
   trap handler |dltrap| as its interpreter, and the CP_CODE field of
   the descriptor points to the name of the primitive as a string.
   When the primitive is first called, the |dltrap| primitive looks up
   the symbol and binds the primitive to its value for future use.
   Finally, it calls the newly-loaded primitive to complete the first
   call.

   Procedure |Load| loads a dynamic library.  Each Oberon module that
   links to a dynamic library should call |DynLink.Load("path")| 
   in its initialization part, and all primitives it defines
   should have a name of the form "*Module_Prim".  The linker sees the
   "*" and takes care of setting up the descriptors to point to
   |dltrap|. *)

(* COPY
#ifdef STUB
#undef DYNLINK
#endif

#ifdef DYNLINK
#define __USE_GNU
#include <dlfcn.h>

extern char *LIB_PATH;
#endif   

void dltrap(value *sp) {
     value *bp = sp;
#ifdef DYNLINK
     value *cp = bp[CP].p;
     char *name = (char * ) cp[CP_CODE].x;
     primitive *prim;

     /* Bind the primitive to the library symbol */
     prim = (primitive * ) dlsym(RTLD_DEFAULT, name);
     if (prim == NULL) liberror(dlerror());
     cp[CP_PRIM].z = prim;

     /* Call the primitive for the first time */
     ( * prim)(sp);
#else
     liberror("dynamic loading is disabled");
#endif
}
*)

(** Load -- load primitives from a shared library *)
PROCEDURE Load*(fname: ARRAY OF CHAR) IS "DynLink_Load";
(* CODE 
#ifdef DYNLINK
     char *fname = (char * ) args[0].x;
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
	liberror(dlerror());
#endif
*)

END DynLink.

