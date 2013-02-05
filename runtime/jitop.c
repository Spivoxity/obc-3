#include "obx.h"
#define MNEMONIC
#include "jit.h"

int vm_debug;

#ifdef DEBUG
void _vm_dbg(const char *fmt, ...) {
     va_list va;

     va_start(va, fmt);
     printf("--- ");
     vprintf(fmt, va);
     printf("\n");
     va_end(va);
}

char *fmt_val(int v) {
     static char buf[16];

     if (v > -1000 && v < 1000)
          sprintf(buf, "%d", v);
     else
          sprintf(buf, "%#x", v);

     return buf;
}

char *fmt_dest(codepoint lab) {
     static char buf[20];

     if (lab->l_lab < 0)
	  return "???";
     else {
	  sprintf(buf, "%d", lab->l_lab);
	  return buf;
     }
}
#endif

void use_label(code_addr loc, codepoint lab) {
     if (lab == NULL) panic("undefined label");

     patch_label(loc, lab);
     if (lab->l_depth < 0)
	  save_stack(lab);
}
     
/* gargs -- generate function arguments */
static void gargs(int n, va_list va) {
     int i;
     reg arg[6];

     for (i = 0; i < n; i++)
	  arg[i] = (reg) va_arg(va, reg);

     g1i(PREP, n);
     for (i = n-1; i >= 0; i--)
	  g1r(ARG, arg[i]);
}

void gcall1(const char *fname, int f, int n, ...) {
     va_list va;

     va_start(va, n);
     gargs(n, va);
     va_end(va);
     vm_dbg("CALL %s", fname);
     vm_gen1i(CALL, f);
}

/* gcallr -- indirect function call */
void gcallr(reg f, int n, ...) {
     va_list va;

     va_start(va, n);
     gargs(n, va);
     va_end(va);
     g1r(CALL, f);
}

void unknown(const char *where, operation op) {
     fprintf(stderr, "Unknown op -- %s %s\n", where, mnemonic[op]);
     exit(2);
}


/* jit_debug -- enable vm debugging */
void jit_debug(value *bp) {
     vm_debug = dflag = bp[HEAD].i;
}

/* vm_alloc -- upcall from vm to allocate code buffer */
void *vm_alloc(int size) {
     return scratch_alloc(size, TRUE);
}
