/*
 * oblink.h
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

#include "config.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include <assert.h>
#include "obcommon.h"
#include "exec.h"

#ifdef DEBUG
/* Small tables to help test auto-growth */
#define INIT_PMEM 8		/* Init space for procedures */
#define INIT_XMEM 128		/* Same for data buffer */
#define INIT_LMEM 16		/* Same for local labels */
#define INIT_SMEM 16		/* Same for symbol table */
#define INIT_MODS 10		/* Init space for modules */
#else
#define INIT_PMEM 256		/* Init space for procedures */
#define INIT_XMEM 2048		/* Same for data buffer */
#define INIT_LMEM 1024		/* Same for local labels */
#define INIT_SMEM 1024		/* Same for symbol table */
#define INIT_MODS 100		/* Init space for modules */
#endif

#define STACK_SIZE (1024 * 1024 - 32)	
				/* Default stack size (bytes) */
#define MIN_STACK 4096		/* Min stack size (bytes) */

typedef enum { ABS, DATA, BSS, CODE, UNDEFINED } segment;

typedef struct phrase *phrase;

typedef struct template *template;

#define MAXMAC 6

struct template {		/* An encoding of an instruction */
     char *t_name;		/* The instruction */
     char *t_pattern;		/* Argument formats */
     int t_lo, t_hi, t_step;	/* Pattern of values for 'N' format */
     int t_size;		/* Total length of instruction */
     int t_oplen;		/* Length of opcode */
     uchar t_op;		/* Opcode */
     char *t_macro[MAXMAC];	/* Macro expansion */
};

#define put1(buf, x) put_int(1, buf, x)
#define put2(buf, x) put_int(2, buf, x)
#define put4(buf, x) put_int(4, buf, x)


EXTERN int dflag;
EXTERN int zflag;		/* Whether to compress the bytecode */
EXTERN bool sflag;		/* Whether to suppress symbol table */
EXTERN bool gflag;		/* Whether to output extra debugging info */
EXTERN bool custom;
EXTERN bool dump;
EXTERN bool linecount;
EXTERN int stack_size;

/* template.c */
extern struct template templates[];
extern short templ_trie[];
extern uchar templ_check[];

/* symtab.c */
typedef struct symbol *symbol;

symbol make_symbol(char *name);
symbol find_symbol(char *name);
char *sym_name(symbol s);
void def_global(symbol s, segment seg, int off, int kind);
void use_global(symbol s, uchar *base, int offset);
int sym_value(symbol s);
bool known(char *name);
void fix_data(uchar *base, int bss);
int write_symtab(void);
void module_data(symbol s, unsigned checksum, int nlines);

void init_labels(void);
void def_label(int n, phrase val);
void sort_labels(void);
phrase find_label(int n);

int find_prim(char *name);
void make_prim(char *name);
void dump_prims(FILE *fp);
extern unsigned prim_check;

/* linker.c */
void init_linker(char *outname, char *interp);
void put_inst(char *name, char *rands[], int nrands);
void gen_inst(char *fmt, ...);
void save_string(char *label, char *string);
void open_output(char *name, char *interp);
void end_linking(void);

void relocate(int loc, int bits);

void put_int(int n, uchar *buf, int x);
int get4(uchar *buf);
void write_string(char *s);
void write_int(int n, int x);

/* util.c */
#include "util.h"
