#include "vm.h"
#include <stdlib.h>
#include <stdio.h>
#include <assert.h>

typedef int (*funcp)(int);

funcp compile(void) {
     code_addr entry;
     vmlabel lab1 = vm_newlab(), lab2 = vm_newlab();
     vmreg r0 = ireg[0], r1 = ireg[1];

     entry = vm_begin("fact", 1);
     vm_gen(GETARG, r0, 0);
     vm_gen(MOV, r1, 1);

     vm_label(lab1);
     vm_gen(BEQ, r0, 0, lab2);
     vm_gen(MUL, r1, r1, r0);
     vm_gen(SUB, r0, r0, 1);
     vm_gen(JUMP, lab1);

     vm_label(lab2);
     vm_gen(MOV, ret, r1);
     vm_gen(RET);

     vm_end();

     return (funcp) entry;
}

funcp compile2(void) {
     code_addr entry;

     vmlabel lab1 = vm_newlab(), lab2 = vm_newlab();
     vmreg r0 = ireg[0], r1 = ireg[1];

     // Use a local to save r0 across the call.
     entry = vm_begin_locals("fact", 1, 4);
     vm_gen(GETARG, r0, 0);

     vm_gen(BNEQ, r0, 0, lab1);
     vm_gen(MOV, ret, 1);
     vm_gen(JUMP, lab2);

     vm_label(lab1);
     vm_gen(STW, r0, base, 0);
     vm_gen(SUB, r1, r0, 1);
     vm_gen(PREP, 1);
     vm_gen(ARG, r1);
     vm_gen(CALL, (int) entry);
     vm_gen(LDW, r0, base, 0);
     vm_gen(MUL, ret, r0, ret);
     
     vm_label(lab2);
     vm_gen(RET);

     vm_end();
     return (funcp) entry;
}

static float a[] = { 3.0, 1.0, 4.0, 1.0, 5.0, 9.0 };

void (*compile3(void))(int, float *) {
     code_addr entry;
     vmlabel lab1 = vm_newlab(), lab2 = vm_newlab();
     vmreg n = ireg[0], i = ireg[1], t = ireg[2], y = ireg[3];
     vmreg s = freg[0], x = freg[1];

     entry = vm_begin("sum", 3);
     vm_gen(GETARG, n, 0);
     vm_gen(GETARG, y, 1);

     vm_gen(MOV, i, 0);
     vm_gen(ZEROF, s);
     vm_label(lab1);
     vm_gen(BGEQ, i, n, lab2);
     vm_gen(LSH, t, i, 2);
     vm_gen(LDW, x, t, vm_addr(a));
     vm_gen(ADDF, s, s, x);
     vm_gen(ADD, i, i, 1);
     vm_gen(JUMP, lab1);
     vm_label(lab2);
     vm_gen(STW, s, y);
     vm_gen(RET);

     vm_end();
     return (void (*)(int, float *)) entry;
}
 
int (*compile4(void))(void) {
     code_addr entry;
     vmreg x = ireg[2], y = ireg[0];

     entry = vm_begin_locals("foo", 0, 4);
     vm_gen(MOV, x, (int) 'A');
     vm_gen(STC, x, base, 0);
     vm_gen(LDC, y, base, 0);
     vm_gen(MOV, ret, y);
     vm_gen(RET);
     return (int (*)(void)) entry;
}

int main(int argc, char *argv[]) {
     funcp fp, fp2;
     void (*fp3)(int, float *);
     int (*fp4)(void);
     int n;
     float y;

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
     fp3(6, &y);
     printf("The sum of that array is %f\n", y);
     printf("The character is %c\n", (char) fp4());
     return 0;
}

void *vm_alloc(int size) {
     void *mem = NULL;
     if (posix_memalign(&mem, 4096, size) < 0) {
          fprintf(stderr, "Allocation failed\n");
          exit(2);
     }
     return mem;
}
