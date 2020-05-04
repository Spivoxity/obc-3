/*
 * gc0.c
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

#include "obx.h"
#include <stdlib.h>

#ifdef USE_BOEHM
#include <gc/gc.h>
#endif

#ifdef HAVE_MMAP
#include <fcntl.h>
#include <sys/mman.h>

#ifdef MACOS
#define MAP_ANONYMOUS MAP_ANON
#define HINT (void *) 0x10000000L
#define MMAP_FLAGS MAP_PRIVATE
#else
#define HINT NULL
#define MMAP_FLAGS MAP_PRIVATE
#endif

static void *grab_chunk(unsigned size) {
     void *p;
     static void *last_addr = HINT;

#ifdef MAP_ANONYMOUS
     p = mmap(last_addr, size, PROT_READ|PROT_WRITE, 
	      MMAP_FLAGS|MAP_ANONYMOUS, -1, 0);
#else
     static int zero_fd = -1;

     if (zero_fd < 0) {
	  zero_fd = open("/dev/zero", O_RDONLY);
	  if (zero_fd < 0) panic("couldn't open /dev/zero");
     }

     p = mmap(last_addr, size, PROT_READ|PROT_WRITE,
              MMAP_FLAGS, zero_fd, 0);
#endif

     if (p == MAP_FAILED) return NULL;
     last_addr = p + size;
     return p;
}
#endif

void *scratch_alloc(unsigned size) {
     void *p = NULL;

#ifdef USE_BOEHM
     p = GC_malloc_uncollectable(size);
#else
     p = malloc(size);
#endif
     
     if (p == NULL) panic("scratch_alloc failed");
     return p;
}

void *scratch_alloc_atomic(unsigned size) {
     void *p = NULL;

#ifdef USE_BOEHM
     p = GC_malloc_atomic_uncollectable(size);
#else
     p = malloc(size);
#endif
     
     if (p == NULL) panic("scratch_alloc_atomic failed");
     return p;
}

void *gc_alloc(value *desc, unsigned size, value *sp) {
#ifdef USE_BOEHM
     value *p = GC_malloc(size+4);
#else
     value *p = malloc(size+4);
#endif
     if (p == NULL) panic("malloc failed");
     (*p).a = address(desc);
     return p+1;
}

int gc_heap_size(void) {
#ifdef USE_BOEHM
     return GC_get_heap_size();
#else
     return 0;
#endif
}

void gc_collect(value *sp) {
#ifdef USE_BOEHM
     GC_gcollect();
#endif
}

void gc_debug(char *flags) { }

void gc_init(void) {
#ifdef USE_BOEHM
     GC_init();
#endif
}

/* vm_alloc -- upcall from vm to allocate code buffer */
void *vm_alloc(int size) {
     void *p = grab_chunk(size);
     if (p == NULL) panic("Failed to allocate JIT memory");
     return p;
}
