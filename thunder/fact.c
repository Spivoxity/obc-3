#include "config.h"
#include "vm.h"
#include <stdlib.h>
#include <stdio.h>
#include <assert.h>

typedef int (*funcp)(int);

funcp compile(void) {
     int entry;
     vmlabel lab1 = vm_newlab(), lab2 = vm_newlab();
     vmreg r0 = vm_ireg[0], r1 = vm_ireg[1];

     entry = vm_begin("fact", 1);
     vm_gen(GETARG, r0, 0);
     vm_gen(MOV, r1, 1);

     vm_label(lab1);
     vm_gen(BEQ, r0, 0, lab2);
     vm_gen(MUL, r1, r1, r0);
     vm_gen(SUB, r0, r0, 1);
     vm_gen(JUMP, lab1);

     vm_label(lab2);
     vm_gen(MOV, vm_ret, r1);
     vm_gen(RET);

     vm_end();

     return (funcp) vm_func(entry);
}

funcp compile2(void) {
     int entry;

     vmlabel lab1 = vm_newlab(), lab2 = vm_newlab();
     vmreg r0 = vm_ireg[0], r1 = vm_ireg[1];

     // Use a local to save r0 across the call.
     entry = vm_begin_locals("fact", 1, 4);
     vm_gen(GETARG, r0, 0);

     vm_gen(BNE, r0, 0, lab1);
     vm_gen(MOV, vm_ret, 1);
     vm_gen(JUMP, lab2);

     vm_label(lab1);
     vm_gen(STW, r0, vm_base, 0);
     vm_gen(SUB, r1, r0, 1);
     vm_gen(PREP, 1);
     vm_gen(ARG, r1);
     vm_gen(CALL, (int) entry);
     vm_gen(LDW, r0, vm_base, 0);
     vm_gen(MUL, vm_ret, r0, vm_ret);
     
     vm_label(lab2);
     vm_gen(RET);

     vm_end();
     return (funcp) vm_func(entry);
}

static float a[] = { 3.0, 1.0, 4.0, 1.0, 5.0, 9.0 };

#include <string.h>

void *vm_literal(int n);

void (*compile3(void))(int, float *) {
     /* On amd64, we need to put the array in addressible storage */
     float *aa = (float *) vm_literal(sizeof(a));
     memcpy(aa, a, sizeof(a));

     int entry;
     vmlabel lab1 = vm_newlab(), lab2 = vm_newlab();
     vmreg n = vm_ireg[0], i = vm_ireg[1], t = vm_ireg[2], y = vm_ireg[3];
     vmreg s = vm_freg[0], x = vm_freg[1];

     entry = vm_begin("sum", 2);
     vm_gen(GETARG, n, 0);
     vm_gen(GETARG, y, 1);

     vm_gen(MOV, i, 0);
     vm_gen(ZEROf, s);
     vm_label(lab1);
     vm_gen(BGE, i, n, lab2);
     vm_gen(LSH, t, i, 2);
     vm_gen(LDW, x, t, vm_addr(*aa));
     vm_gen(ADDf, s, s, x);
     vm_gen(ADD, i, i, 1);
     vm_gen(JUMP, lab1);
     vm_label(lab2);
     vm_gen(STW, s, y);
     vm_gen(RET);

     vm_end();
     return (void (*)(int, float *)) vm_func(entry);
}
 
int (*compile4(void))(void) {
     int entry;
     vmreg x = vm_ireg[2], y = vm_ireg[0];

     entry = vm_begin_locals("foo", 0, 4);
     vm_gen(MOV, x, (int) 'A');
     vm_gen(STB, x, vm_base, 0);
     vm_gen(LDB, y, vm_base, 0);
     vm_gen(MOV, vm_ret, y);
     vm_gen(RET);
     return (int (*)(void)) vm_func(entry);
}

int main(int argc, char *argv[]) {
     funcp fp, fp2;
     void (*fp3)(int, float *);
     int (*fp4)(void);
     int n;
     float *y = vm_literal(sizeof(float));

     vm_debug = 2;
     printf("Compiling fac1:\n");
     fp = compile();
     printf("Compiling fac2:\n");
     fp2 = compile2();
     printf("Compiling sum:\n");
     fp3 = compile3();
     printf("Compiling foo:\n");
     fp4 = compile4();

     n = (argc > 1 ? atoi(argv[1]) : 10);
     printf("The factorial of %d is %d\n", n, fp(n));
     printf("The factorial of %d is %d\n", n, fp2(n));
     fp3(6, y);
     printf("The sum of that array is %f\n", *y);
     printf("The character is %c\n", (char) fp4());
     return 0;
}

#ifndef M64X32

void *vm_alloc(int size) {
     void *mem = NULL;
     if (posix_memalign(&mem, 4096, size) < 0) {
          fprintf(stderr, "Allocation failed\n");
          exit(2);
     }
     return mem;
}

#else

#include <sys/mman.h>

void *vm_alloc(int size) {
     void *p;
     static void *last_addr = NULL;

     p = mmap(last_addr, size, PROT_READ|PROT_WRITE, 
	      MAP_PRIVATE|MAP_32BIT|MAP_ANONYMOUS, -1, 0);

     if (p == MAP_FAILED) return NULL;
     if ((((unsigned long) p) & ~0x7fffffff) != 0) {
          fprintf(stderr, "inaccessible memory allocated at %p", p);
          exit(2);
     }
     last_addr = p + size;
     return p;
}

#endif
