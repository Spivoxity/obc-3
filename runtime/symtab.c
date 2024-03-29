/*
 * symtab.c
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

#include "oblink.h"

/* This module implements two completely independent symbol tables:
   one for global symbols, and another for labels used in branches.
   Global symbols have symbolic names, and are used only in the data
   segment. Labels have names that are integers. */


/* GLOBAL SYMBOLS */

struct _symbol {
     const char *s_name;	/* Name of the symbol */
     segment s_seg;		/* Segment, or UNDEFINED */
     int s_kind;		/* Kind of symbol -- X_PROC, etc. */
     int s_value;		/* Numeric value */
     unsigned s_index;          /* Index as a local label */
     unsigned s_check;		/* Checksum for module */
     int s_nlines;		/* Line count for module */
     symbol s_next;		/* Next in hash chain */
     int s_uchain;		/* Start of use chain in data segment */
     char *s_file;		/* Source file that uses the symbol */
};

#define HSIZE 1024

static symbol stable[HSIZE];

static growdecl(dict);
#define dict growbuf(dict, symbol)
#define ndict growsize(dict)

/* make_symbol -- create a symbol, but don't put it in the hash table */
symbol make_symbol(const char *name) {
     symbol s = 
	  (symbol) must_alloc(sizeof(struct _symbol), "symbol table entry");
     s->s_name = must_strdup(name);
     s->s_seg = UNDEFINED;
     s->s_kind = X_NONE;
     s->s_value = -1;
     s->s_next = NULL;
     s->s_index = 0;
     s->s_uchain = -1;
     s->s_check = s->s_nlines = 0;
     s->s_file = NULL;

     buf_grow(dict);
     dict[ndict++] = s;
     return s;
}

static symbol lookup(const char *name, mybool create) {
     if (dict == NULL)
	  buf_init(dict, INIT_SMEM, 1, symbol, "symbol table");

     unsigned h = 0;
     for (const char *p = name; *p != '\0'; p++) h = 5 * h + *p;
     h %= HSIZE;

     symbol s;
     for (s = stable[h]; s != NULL; s = s->s_next)
	  if (strcmp(name, s->s_name) == 0)
	       return s;

     if (create) {
	  s = make_symbol(name);
	  s->s_next = stable[h];
	  stable[h] = s;
     }

     return s;
}

/* find_symbol -- find a global symbol, or create one if necessary */
symbol find_symbol(const char *name) {
     return lookup(name, TRUE);
}

/* known -- test if a symbol has been entered */
mybool known(const char *name) {
     symbol s = lookup(name, FALSE);
     return (s != NULL);
}

const char *sym_name(symbol s) {
     return s->s_name;
}

/* sym_value -- compute value of global symbol */
int sym_value(symbol s) {
     if (s->s_file == NULL) s->s_file = err_file;

     if (s->s_seg == UNDEFINED) {
	  err_file = s->s_file;
	  error("undefined symbol %s", s->s_name);
	  s->s_seg = ABS;
     }

     return s->s_value;
}

#ifdef DEBUG
static const char *seg_name[] = { 
     "abs", "data", "bss", "code", "undefined"
};
#endif

/* def_global -- set value of a global symbol */
void def_global(symbol s, segment seg, int off, int kind) {
     if (s->s_seg != UNDEFINED)
	  error("multiply defined symbol %s", s->s_name);

     s->s_seg = seg;
     s->s_value = off;
     s->s_kind = kind;

#ifdef DEBUG
     if (dflag)
	  printf("Symbol %s = %d(%s)\n", 
                 s->s_name, s->s_value, seg_name[s->s_seg]);
#endif
}

/* Uses of globals are linked in chains, so the values can be patched at
the last minute before the data segment is output.  Because the buffer for
the data segment may grow and be relocated, we must store the links as
offsets from the start of the buffer. */

/* use_global -- add location to use chain for a global symbol */
void use_global(symbol s, uchar *base, int offset) {
     if (s->s_file == NULL) s->s_file = err_file;
     *((int *) &base[offset]) = s->s_uchain;
     s->s_uchain = offset;
}

/* fix_data -- fix up global refs in the data segment */
void fix_data(uchar *base, int bss) {
     /* Shift BSS symbols by offset bss */
     for (int i = 0; i < ndict; i++) {
	  symbol s = dict[i];
	  if (s->s_seg == BSS) s->s_value += bss;
     }

     /* Fix up each symbol */
     for (int i = 0; i < ndict; i++) {
	  symbol s = dict[i];
	  int val;

          if (s->s_uchain == -1) continue;

	  if (dflag > 0) printf("Fixing %s\n", s->s_name);

	  val = sym_value(s);

	  /* Run along the use chain, inserting the value */
	  for (int u = s->s_uchain, v; u != -1; u = v) {
	       v = *((int *) &base[u]);
	       put4(&base[u], val);
	       relocate(u, (s->s_seg == ABS ? R_WORD : R_ADDR));
	  }
     }
}

/* module_data -- add data for module */
void module_data(symbol s, unsigned checksum, int nlines) {
     s->s_check = checksum;
     s->s_nlines = nlines;
}

static int cf_syms(symbol *a, symbol *b) {
     int z = (*a)->s_kind - (*b)->s_kind;
     if (z == 0) z = (*a)->s_value - (*b)->s_value;
     return z;
}

/* write_symtab -- write the symbol table */
int write_symtab(void) {
     int nwritten = 0;

     qsort(dict, ndict, sizeof(symbol), 
	   (int (*)(const void *, const void *)) cf_syms);

     for (int i = 0; i < ndict; i++) {
	  symbol s = dict[i];

	  if (s->s_kind == X_SYM || s->s_kind == X_NONE) continue;

	  write_int(4, s->s_kind);
	  write_string(s->s_name);
	  write_int(4, s->s_value);

	  if (s->s_kind == X_MODULE) {
	       write_int(4, s->s_check);
	       write_int(4, s->s_nlines);
	  }

	  nwritten++;
     }

     return nwritten;
}

void preload_symtab(FILE *binfp) {
     qsort(dict, ndict, sizeof(symbol), 
	   (int (*)(const void *, const void *)) cf_syms);
     
     fprintf(binfp,
             "const struct { char *name; int kind; int val; }"
             " preload_syms[] = {\n");

     for (int i = 0; i < ndict; i++) {
          symbol s = dict[i];
          switch (s->s_kind) {
          case X_PROC:
               fprintf(binfp, "     { \"%s\", X_PROC, %u },\n",
                       s->s_name, s->s_value);
               break;
          case X_MODULE:
               fprintf(binfp, "     { \"%s\", X_MODULE, %u },\n",
                       s->s_name, s->s_value);
               break;
          }
     }

     fprintf(binfp, "};\n\n");
}
               

/* LOCAL LABELS */

/* The table contains local symbols for the current procedure only.
   As the procedure is initially assembled into the linker's buffer,
   the value of each label is defined as its location in the buffer.
   When the procedure is complete, we replace each use of a 
   label by the corresponding value.  The values are turned into 
   offsets as the code is output. */ 

struct _locdef {
     symbol l_lab;
     phrase l_val;
};

static growdecl(locdefs);
#define locdefs growbuf(locdefs, struct _locdef)
#define n_locs growsize(locdefs)

void init_labels(void) {
     if (locdefs == NULL)
	  buf_init(locdefs, INIT_LMEM, 1, struct _locdef, "labels");
     n_locs = 0;
}

int make_label(symbol s) {
     int n = s->s_index;

     if (s->s_index >= n_locs || locdefs[s->s_index].l_lab != s) {
          buf_grow(locdefs);
          n = n_locs++;
          locdefs[n].l_lab = s;
          locdefs[n].l_val = NULL;
          s->s_index = n;
     }

     return n;
}

const char *label_name(int n) {
     return sym_name(locdefs[n].l_lab);
}

void def_label(symbol s, phrase val) {
     int n = make_label(s);
     if (locdefs[n].l_val != NULL)
          error("multiply defined label %s", s->s_name);
     locdefs[n].l_val = val;
}

phrase find_label(int n) {
     phrase val = locdefs[n].l_val;
     if (val == NULL)
          error("undefined label %s", locdefs[n].l_lab->s_name);
     return val;
}
