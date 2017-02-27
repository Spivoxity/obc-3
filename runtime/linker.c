/*
 * linker.c
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

#include <ctype.h>
#include "oblink.h"
#include "keiko.h"

static FILE *binfp;		/* File for code output */

/* binwrite -- output code */
static void binwrite(void *buf, int size) {
     int UNUSED nwritten = fwrite(buf, 1, size, binfp);
}

/* hexchar -- convert character from two-digit hex */
static char hexchar(char *s) {
     char buf[3];

     buf[0] = s[0]; buf[1] = s[1]; buf[2] = '\0';
     return (char) strtoul(buf, NULL, 16);
}

static int iloc = 0, bloc = 0;	/* Sizes of code, bss segments */
static int nmods = 0, nprocs = 0; /* Number of modules and procedures */
static symbol this_module;	/* Current module */
static symbol this_proc;	/* Current procedure */
static int proc_start;		/* Start of procedure in dbuf */
static int code_size;		/* Size of bytecode for procedure */

/* Instructions are stored as 'phrases' in abuf, a doubly-linked list.
   Each phrase has a tentative assignment of a template, which describes 
   at least what arguments there should be; before the code is output, the
   template may be replaced by one with bigger fields in order to make the 
   arguments fit.  The code for a procedure is built up in abuf and output 
   at the end of the procedure. */

struct _phrase {		/* An instruction in the assembler */
     const char *q_name;	/* Instruction name */
     template q_templ;		/* Best estimate of template */
     int q_arg[MAXARGS];	/* Arguments */
     int q_addr;		/* Estimated address from start of proc */
     symbol q_sym;		/* Symbol for this address */
     phrase q_target;		/* Branch target */
     phrase q_prev, q_next;	/* Links in the list */
};

phrase abuf;

#define for_phrases(q) \
     for (q = abuf->q_next; q != abuf; q = q->q_next)	

mempool pool;

static phrase alloc_phrase(void) {
     return (phrase) pool_alloc(&pool, sizeof(struct _phrase));
}

static void init_abuf(void) {
     pool_reset(&pool);
     abuf = alloc_phrase();
     abuf->q_name = (char *) "*dummy*";
     abuf->q_templ = NULL;
     abuf->q_addr = 0;
     abuf->q_sym = NULL;
     abuf->q_target = NULL;
     abuf->q_prev = abuf->q_next = abuf;
}

static growdecl(dbuf);
#define dbuf growbuf(dbuf, uchar)
#define dloc growsize(dbuf)

static growdecl(rbuf);
#define rbuf growbuf(rbuf, unsigned)
#define rloc growsize(rbuf)

/* relocate -- record relocation bits */
void relocate(int loc, int bits) {
     /* Each byte of relocation info covers CODES_PER_BYTE words */
     int index = loc/(WORD_SIZE * CODES_PER_WORD);
     int shift = loc/WORD_SIZE % CODES_PER_WORD * BITS_PER_CODE;

     if (rloc < index+1) rloc = index+1;
     buf_grow(rbuf);
     rbuf[index] = (rbuf[index] & ~(CODE_MASK << shift)) | (bits << shift);
#ifdef DEBUG
     if (dflag) printf("Reloc %d %d %#08x\n", loc, bits, rbuf[index]);
#endif
}

static void put_value(int addr, int value, int reloc) {
     /* We carefully store all 4-byte values in dbuf in
        machine-independent byte order: little-endian even if the host
        is a big-endian machine. The value reloc determines how the
        value should be relocated when the program is loaded by obx. */
     put4(&dbuf[addr], value);
     relocate(addr, reloc);
}

static int const_value(char *s) {
     /* We must allow both signed and unsigned (especially hex)
	constants, so negative numbers must be treated separately.
	Note that strtol is specified to truncate to MAXINT on
	overflow, not to operate mod 2^32. */

     if (s[0] == '-')
	  return strtol(s, NULL, 0);
     else
	  return strtoul(s, NULL, 0);
}

static void data_value(int value, int reloc) {
     buf_grow(dbuf);
     put_value(dloc, value, reloc);
     dloc += 4;
}

static void data_word(char *s) {
     buf_grow(dbuf);
     if (isdigit((int) s[0]) || s[0] == '-')
	  put_value(dloc, const_value(s), R_WORD);
     else
	  use_global(find_symbol(s), dbuf, dloc);
     dloc += 4;
}

static void put_string(char *str) {
     char *s = str;
     do { 
	  buf_grow(dbuf); 
	  dbuf[dloc++] = *s;
     } while (*s++ != '\0');
     dloc = align(dloc, 4);
}


/* Constant pool */

static growdecl(const_sym);
#define const_sym growbuf(const_sym, symbol)
#define nconsts growsize(const_sym)

#define get_const(n) get4(dbuf + proc_start + 4 * (CP_CONST+(n))) 

static int find_const(int value, symbol sym) {
     int i;

     for (i = 0; i < nconsts; i++) {
	  if (sym == NULL
	      ? (const_sym[i] == NULL && get_const(i) == value)
	      : const_sym[i] == sym)
	       return i;
     }

     i = nconsts++;
     buf_grow(const_sym);
     const_sym[i] = sym;
     buf_grow(dbuf);

     if (sym == NULL)
	  put_value(dloc, value, R_WORD);
     else
	  use_global(sym, dbuf, dloc);

     dloc += 4;
     return i;
}

static int find_dconst(int val0, int val1) {
     int i;

     for (i = 0; i < nconsts-1; i++) {
	  if (const_sym[i] == NULL && const_sym[i+1] == NULL
	      && get_const(i) == val0 && get_const(i+1) == val1)
	       return i;
     }

     i = nconsts; nconsts += 2;
     buf_grow(const_sym);
     const_sym[i] = const_sym[i+1] = NULL;
     data_value(val0, R_WORD);
     data_value(val1, R_WORD);

     return i;
}

static int make_const(char *s) {
     if (isdigit((int) s[0]) || s[0] == '-')
	  return find_const(const_value(s), NULL);
     else
	  return find_const(0, find_symbol(s));
}


/* Instruction templates */

/* find_template -- find first template for instruction */
static template find_template(const char *name) {
     const char *s = name;
     int q = 0;
     char ch;

     /* The templates are organised in a trie */

     do {
	  ch = *s++ & 0x7f;

	  if (q+ch < 0 || q+ch >= NTRIE || templ_check[q+ch] != ch) 
	       panic("*no template found for %s", name);

	  q = templ_trie[q+ch];
     } while (ch != '\0');

     return &templates[q];
}

/* fits -- test if an integer fits in a certain number of bits */
static mybool fits(int x, int n) {
     int max = 1 << (n-1);
     return (-max <= x && x < max);
}

/* fix_labels -- compute target for jump */
static void fix_labels(phrase q) {
     const char *p = q->q_templ->t_pattern;
     int j;
     
     for (j = 0; p[j] != '\0'; j++)
	  if (p[j] == 'R' || p[j] == 'S') 
	       q->q_target = find_label(q->q_arg[j]);
}

/* displacement -- calculate branch displacement */
static int displacement(phrase q) {
     /* Phrase |q| is a branch instruction.  The signed displacement
        is the distance from the opcode to the target. */
     return (q->q_target->q_addr - q->q_addr);
}

/* match -- test whether a template matches its arguments */
static mybool match(phrase q, template t) {
     /* Just check the last operand */
     int n = strlen(t->t_pattern);
     const char *p = t->t_pattern;
     int *a = q->q_arg;

     if (n == 0) return TRUE;

     switch (p[n-1]) {
     case 'N':
	  { int val = a[n-1];
	    return (val >= t->t_lo && val <= t->t_hi 
		    && (val - t->t_lo) % t->t_step == 0); }
     case '1':
     case 'K':
	  return fits(a[n-1], 8);
     case '2':
     case 'L':
	  return fits(a[n-1], 16);
     case 'R':
	  return fits(displacement(q), 16);
     case 'S':
	  return fits(displacement(q), 8);
     default:
	  return TRUE;
     }
}

#ifdef DEBUG
static void print_args(phrase q) {
     const char *patt = q->q_templ->t_pattern;
     int j;

     for (j = 0; patt[j] != '\0'; j++) {
	  switch (patt[j]) {
	  case '1':
	  case '2':
	  case 'N':
	  case 'K':
	  case 'L':
	       printf(" %d", q->q_arg[j]); break;
	  case 'R':
	  case 'S':
	       printf(" %+d", displacement(q)); break;
	  default:
	       printf(" ???");
	  }
     }
}
#endif

static phrase do_template(template t, char *rands[], phrase buf, int cxt[]);

/* expand -- replace macro by its expansion */
static phrase expand(phrase q) {
     static char buf[128];
     char *words[10];
     template t = q->q_templ, t1;
     unsigned int i, n;
     phrase r = q->q_prev, q1;

     for (i = 0; t->t_macro[i] != NULL; i++) {
          strcpy(buf, t->t_macro[i]);
	  n = split_line(buf, words);
	  t1 = find_template(words[0]);
	  if (strlen(t1->t_pattern) != n-1 || t->t_size < 0) 
	       panic("*macro expansion failed");

	  /* Insert expansion before original phrase */
	  q1 = do_template(t1, &words[1], q, q->q_arg);
	  fix_labels(q1);
     }

     /* Delete the original */
     q->q_prev->q_next = q->q_next;
     q->q_next->q_prev = q->q_prev;

     return r->q_next;
}     

/* check_matches -- revise choice of templates, return TRUE if ok already */
static mybool check_matches(void) {
     phrase q;
     mybool ok = TRUE;

     for (q = abuf->q_next; q != abuf; ) {
	  template t = q->q_templ;

	  if (t->t_macro[0] != NULL) {
	       /* A macro instruction: expand it */
	       q = expand(q);
	       ok = FALSE;
	  } else if (! match(q, t)) {
	       t++;

	       if (t >= &templates[NTEMPLATES] || t->t_name != NULL) {
		    panic("*no template fits %s", q->q_name);
	       }

	       q->q_templ = t;
	       ok = FALSE;
	  } else {
	       q = q->q_next;
	  }
     }
       
     return ok;
}

/* assemble -- assemble instructions */
static void assemble(void) {
     mybool ok;
     phrase q;
     int trial = 0;

     for_phrases (q) fix_labels(q);

     /* A tentative assignment of templates has already been computed,
	but the arguments may not fit in the field sizes assigned.  So
	now we repeatedly revise the assignment until all arguments fit.
	Changing the assignment will increase the size of some instructions,
	perhaps making branches longer so that they no longer fit either
	-- that's why iteration is necessary.

	The invariant is that there is no feasible choice of templates that
	makes any instruction smaller than it is in the current assignment.
	The variant is the total number of templates that remain to be tried.
	Correctness of the algorithm follows from the fact that making one 
	instruction larger cannot allow another to be smaller. */

     do {
          int a = 0;
	  trial++;
#ifdef DEBUG
	  if (dflag > 0)
	       printf("Checking templates (pass %d)\n", trial);
#endif	  

	  /* Calculate address of each instruction */
          for_phrases (q) {
               q->q_addr = a;
               a += q->q_templ->t_size;
          }

          code_size = a;
	  ok = check_matches();	/* Revise template choices */
     } while (!ok);
}

/* make_binary -- output binary code */
static void make_binary(void) {
     phrase q;
     int j;

     for_phrases (q) {
	  template t = q->q_templ;
	  const char *p = t->t_pattern;
	  int *a = q->q_arg;

#ifdef DEBUG
	  if (dflag > 0) {
	       printf("%d: %s(%s)", q->q_addr, q->q_name, p);
	       print_args(q);
	       printf("\n");
	  }
#endif

	  if (q->q_sym != NULL)
	       def_global(q->q_sym, CODE, iloc + q->q_addr, X_LINE);

	  if (p[0] == 'N')
	       write_int(1, t->t_op + (a[0] - t->t_lo)/t->t_step);
	  else if (t->t_oplen > 0) 
	       binwrite(&t->t_op, t->t_oplen);

	  for (j = 0; p[j] != '\0'; j++) {
	       switch (p[j]) {
	       case 'N':
		    break;
	       case '1': 
	       case 'K':
		    write_int(1, a[j]); break;
	       case '2':
	       case 'L': 
		    write_int(2, a[j]); break;
	       case 'R': 
		    write_int(2, displacement(q)); break;
	       case 'S': 
		    write_int(1, displacement(q)); break;
	       default:  
		    panic("*bad template %c", p[j]);
	       }
	  }
     }
}

static int get_arg(char tmpl, char *rand, template t, int cxt[]) {
     if (rand[0] == '$' && cxt != NULL)
          return cxt[rand[1] - 'a'];

     switch (tmpl) {
     case '1':
     case '2':
     case 'N':
	  if (isdigit((int) rand[0]) || rand[0] == '-')
	       return const_value(rand);
	  else
	       return sym_value(find_symbol(rand));

     case 'R':
     case 'S':
	  return make_label(find_symbol(rand));

     case 'K':
     case 'L':
	  return make_const(rand);

     default:
	  panic("*bad template %c for %s", tmpl, t->t_name);
	  return 0;
     }
}

/* do_template -- enter an instruction */
static phrase do_template(template t, char *rands[], phrase rgt, int cxt[]) { 
     /* Template t determines the number and kinds of operands for the
	instruction; depending on the values of the operands, it may or
	may not end up actually matching the instruction. */

     phrase q = alloc_phrase();
     phrase lft = rgt->q_prev;
     const char *patt = t->t_pattern;
     int i;

     q->q_name = t->t_name;
     q->q_templ = t;
     for (i = 0; patt[i] != '\0'; i++) 
	  q->q_arg[i] = get_arg(patt[i], rands[i], t, cxt);
     q->q_addr = 0;
     q->q_sym = NULL;
     q->q_target = NULL;
     q->q_prev = lft; q->q_next = rgt;
     lft->q_next = rgt->q_prev = q;
     return q;
}

/* MARK pseudo-instructions generate no code, and are used to place labels,
   line numbers, etc. */
struct _template mark = {
     "*MARK*", "", 0, 0, 0, 0, 0, 0, { NULL }
};

static phrase put_mark(symbol s) {
     phrase q = do_template(&mark, NULL, abuf, NULL);
     q->q_sym = s;
     return q;
}

/* const_head -- start of constant pool */
static void const_head(int prim, int code, int reloc, 
		       int frame, int stack, char *map) {
     data_value(prim, R_SUBR);	/* Primitive */
     data_value(code, reloc);	/* Entry point */
     data_value(0, R_WORD);	/* Code size */
     data_value(frame, R_WORD);	/* Frame size in words */
     data_value(stack, R_WORD); /* Stack size in words */
     data_word(map);		/* Frame map */
     data_value(0, R_WORD);	/* Stack map table */
}

typedef struct {
     phrase sm_addr;		/* Pointer to the JPROC instruction */
     char *sm_text;		/* Symbol or numeric value */
} stackmap;

static growdecl(smbuf);
#define smbuf growbuf(smbuf, stackmap)
#define smp growsize(smbuf)

/* fix_stackmaps -- fix up the stack maps for the current procedure */
static void fix_stackmaps(void) {
     int i;

     if (smp == 0) return;

     /* Fill in the address of the table in the constant pool */
     put_value(proc_start + 4*CP_STKMAP, dloc, R_DATA);

     /* Create the table itself */
     for (i = 0; i < smp; i++) {
	  stackmap *sm = &smbuf[i];

	  /* The return address for the call: '+1' to allow for the space
	     occupied by the JPROC instruction */
	  data_value(iloc + sm->sm_addr->q_addr + 1, R_CODE);

	  /* The stack map */
	  data_word(sm->sm_text);
     }

     data_value(0, R_WORD);
}

typedef struct {
     int h_begin, h_end;	/* Scope of handler */
     symbol h_excep;		/* Exception */
     phrase h_body;		/* Handler code */
} handler;

/* check_inproc -- panic if not in a procedure */
static void check_inproc(const char *opcode) {
     if (this_proc == NULL)
	  panic("*%s occurs outside any procedure", opcode);
}

/* do_directive -- process a directive */
static void do_directive(const char *dir, int n, char *rands[], int nrands) {
     int i;
     union { int n; float f; } fcvt;
     dblbuf dcvt;

     switch (n) {
     case D_LABEL:
	  check_inproc(dir);
	  /* Each label is defined as the |abuf| index of its target */
	  def_label(find_symbol(rands[0]), put_mark(NULL));
	  break;

     case D_STRING:
	  for (i = 0; rands[0][2*i] != '\0'; i++) {
	       buf_grow(dbuf);
	       dbuf[dloc++] = hexchar(&rands[0][2*i]);
	  }
	  dloc = align(dloc, 4);
	  break;

     case D_CONST:
	  check_inproc(dir);
	  if ((isdigit((int) rands[0][0]) || rands[0][0] == '-')
	      && fits(i = const_value(rands[0]), 16))
	       gen_inst("PUSH %d", i);
	  else
	       gen_inst("LDKW %d", make_const(rands[0]));
	  break;

     case D_GLOBAL:
	  check_inproc(dir);
	  gen_inst("LDKW %d", make_const(rands[0]));
	  break;

     case D_FCONST:
	  check_inproc(dir);
	  fcvt.f = atof(rands[0]);
	  gen_inst("LDKF %d", find_const(fcvt.n, NULL));
	  break;

     case D_DCONST:
	  check_inproc(dir);
	  dcvt.d = atof(rands[0]);
	  gen_inst("LDKD %d", find_dconst(dcvt.n.lo, dcvt.n.hi));
	  break;

     case D_QCONST:
	  check_inproc(dir);
	  dcvt.q = strtoll(rands[0], NULL, 0);
	  gen_inst("LDKQ %d", find_dconst(dcvt.n.lo, dcvt.n.hi));
	  break;

     case D_WORD:
	  data_word(rands[0]);
	  break;

     case D_GLOVAR:
	  def_global(find_symbol(rands[0]), BSS, bloc, X_DATA);
	  bloc = align(bloc + strtoul(rands[1], NULL, 0), 4);
	  break;

     case D_MODULE:
	  nmods++;
	  this_module = find_symbol(rands[0]);
	  def_global(this_module, DATA, dloc, X_MODULE);
	  module_data(this_module, strtoul(rands[1], NULL, 0), 
		      strtol(rands[2], NULL, 0));
	  break;

     case D_PRIMDEF:
	  nprocs++;
	  dloc = align(dloc, 8);
	  def_global(find_symbol(rands[0]), DATA, dloc, X_PROC);
	  const_head(DLTRAP, dloc + 4*CP_CONST, R_DATA, 
		     atoi(rands[2]), 0, rands[3]);
	  put_string(rands[1]);
	  break;

     case D_PROC:
	  nprocs++;
	  dloc = align(dloc, 8);
	  this_proc = find_symbol(rands[0]);
          proc_start = dloc;
	  def_global(this_proc, DATA, proc_start, X_PROC);
	  const_head(INTERP, iloc, R_CODE, atoi(rands[1]), 
		     atoi(rands[2]), rands[3]);

          init_abuf();
	  init_labels();
	  nconsts = 0;
	  smp = 0;
	  break;

     case D_STKMAP:
	  /* Stack map for a procedure call */
	  check_inproc(dir);
	  buf_grow(smbuf);
	  smbuf[smp].sm_addr = put_mark(NULL);
	  smbuf[smp].sm_text = must_strdup(rands[0]);
	  smp++;
	  break;

     case D_END:
	  /* End of procedure body */
	  check_inproc(dir);
	  assemble();		/* Finally choose templates */
	  fix_stackmaps();	/* Compile the stack maps */
	  make_binary();	/* Output the code */
	  put_value(proc_start + 4*CP_SIZE, code_size, R_WORD);
	  iloc += code_size;
	  this_proc = NULL;
	  break;

     case D_IMPORT:
     case D_ENDHDR:
	  /* Ignore directives that appear in the file header */
	  break;

     case D_DEFINE:
	  def_global(find_symbol(rands[0]), DATA, dloc, X_DATA);
	  break;

     case D_LINE:
	  check_inproc(dir);

	  if (gflag) {
	       char buf[64];
	       sprintf(buf, "%s.%s", sym_name(this_module), rands[0]);
	       put_mark(make_symbol(buf));
	  }

	  if (linecount) 
	       put_inst("LNUM", rands, nrands);

	  break;

     default:
	  panic("*unknown directive %s (%d)", dir, n);
     }
}

/* put_inst -- process one instruction or directive */
void put_inst(const char *name, char *rands[], unsigned nrands) {
     template t = find_template(name);
     unsigned i;

     if (nrands != strlen(t->t_pattern)) {
	  fprintf(stderr, "Instruction: %s", name);
	  for (i = 0; i < nrands; i++)
	       fprintf(stderr, " %s", rands[i]);
	  fprintf(stderr, ", File: %s\n", err_file);
	  panic("*%s needs %d operands, got %d", 
		name, strlen(t->t_pattern), nrands);
     }

     if (t->t_size < 0)
	  do_directive(t->t_name, t->t_op, rands, nrands);
     else {
	  check_inproc(name);
	  do_template(t, rands, abuf, NULL);
     }
}

/* gen_inst -- generate an instruction from text */
void gen_inst(const char *fmt, ...) {
     char line[80];
     char *words[10];
     int nwords;

     va_list ap;

     va_start(ap, fmt);
     vsprintf(line, fmt, ap);
     strcat(line, "\n");
     va_end(ap);

     nwords = split_line(line, words);
     put_inst(words[0], &words[1], nwords-1);
}

/* save_string -- save a string in the data segment */
void save_string(const char *label, char *str) {
     def_global(find_symbol(label), DATA, dloc, X_DATA);
     put_string(str);
}


/* Object file output */

static int start;		/* Starting offset of binary */

void init_linker(char *outname, char *interp) {
     buf_init(dbuf, INIT_XMEM, 4, uchar, "data");
     buf_init(rbuf, INIT_XMEM/(WORD_SIZE * CODES_PER_WORD), 
	      1, unsigned, "relocation");
     buf_init(smbuf, 16, 1, stackmap, "stack maps");
     buf_init(const_sym, 256, 1, symbol, "constant pool");

     binfp = fopen(outname, "wb");
     if (binfp == NULL) {
	  perror(outname);
	  exit(2);
     }

     if (interp != NULL) 
	  fprintf(binfp, "#!%s\n", interp);

     start = ftell(binfp);
}

/* end_linking -- write later parts of object file */
void end_linking(void) {
     trailer t;
     int fsize, csize, symcount = 0, nwritten;
     const char *magic = MAGIC;

     csize = ftell(binfp) - start;
     if (csize != iloc) {
	  fprintf(stderr, "csize = %d, iloc = %d\n", csize, iloc);
	  panic("*Wrong code size");
     }

     fix_data(dbuf, dloc);
     rloc = (dloc/WORD_SIZE+CODES_PER_WORD-1)/CODES_PER_WORD;
     buf_grow(rbuf);

     binwrite(dbuf, dloc);
     binwrite(rbuf, rloc * sizeof(unsigned));
     if (!sflag) symcount = write_symtab();

     fsize = ftell(binfp) + sizeof(trailer);

#define sym_val(x) (known(x) ? sym_value(find_symbol(x)) : 0)

     /* Trailer */
     strncpy((char *) t.magic, magic, 4);
     put4(t.sig, SIG);
     put4(t.primsig, 0);
     put4(t.start, start - fsize);
     put4(t.entry, sym_val("MAIN"));
     put4(t.gcmap, sym_val("GCMAP"));
     put4(t.libdir, sym_val("LIBDIR"));
     put4(t.segment[S_CODE], iloc);
     put4(t.segment[S_DATA], dloc);
     put4(t.segment[S_BSS], bloc);
     put4(t.segment[S_STACK], stack_size);
     put4(t.nprocs, (sflag ? 0 : nprocs));
     put4(t.nmods, (sflag ? 0 : nmods));
     put4(t.nsyms, symcount);
     nwritten = fwrite(&t, sizeof(trailer), 1, binfp);
     if (nwritten < 1)
	  panic("Couldn't write trailer");

     fclose(binfp);
}


/* Routines for writing values in machine-independent byte order */

void put_int(int n, uchar *buf, int x) {
     int i;

     for (i = 0; i < n; i++)
	  buf[i] = (x >> (8*i)) & 0xff;
}

int get4(uchar *buf) {
     return buf[0] + (buf[1] << 8) + (buf[2] << 16) + (buf[3] << 24);
}

void write_string(const char *s) {
     binwrite((void *) s, strlen(s)+1);
}

void write_int(int n, int x) { 
     uchar buf[4]; 
     put_int(n, buf, x); 
     binwrite(buf, n);
}
