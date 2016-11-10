#include "vm.h"
#include <stdlib.h>
#include <stdio.h>

typedef int (*funcp)(int);

funcp compile(void) {
     code_addr entry;
     vmreg xV0 = vreg[0], xV1 = vreg[1];
     vmlabel lab1 = vm_newlab(), lab2 = vm_newlab();

     entry = vm_begin("fact", 1);
     vm_gen2ri(GETARG, xV0, 0);
     vm_gen2ri(MOV, xV1, 1);

     vm_label(lab1);
     vm_gen3rij(BEQ, xV0, 0, lab2);
     vm_gen3rrr(MUL, xV1, xV1, xV0);
     vm_gen3rri(SUB, xV0, xV0, 1);
     vm_gen1j(JUMP, lab1);

     vm_label(lab2);
     vm_gen2rr(MOV, ret, xV1);
     vm_gen0(RET);

     vm_end();

     return (funcp) entry;
}

funcp compile2(void) {
     code_addr entry;
     vmlabel lab1 = vm_newlab(), lab2 = vm_newlab();
     vmreg xV0 = vreg[0], xV1 = vreg[1];

     entry = vm_begin("fact", 1);
     vm_gen2ri(GETARG, xV0, 0);

     vm_gen3rij(BNEQ, xV0, 0, lab1);
     vm_gen2ri(MOV, ret, 1);
     vm_gen1j(JUMP, lab2);
     
     vm_label(lab1);
     vm_gen3rri(SUB, xV1, xV0, 1);
     vm_gen1i(PREP, 1);
     vm_gen1r(ARG, xV1);
     vm_gen1i(CALL, (int) entry);
     vm_gen3rrr(MUL, ret, xV0, ret);

     vm_label(lab2);
     vm_gen0(RET);

     vm_end();

     return (funcp) entry;
}

int main(int argc, char *argv[]) {
     funcp fp, fp2;
     int n;

     vm_debug = 2;
     fp = compile(); fp2 = compile2();
     n = (argc > 1 ? atoi(argv[1]) : 10);
     printf("The factorial of %d is %d\n", n, (*fp)(n));
     printf("The factorial of %d is %d\n", n, (*fp2)(n));
     return 0;
}

void *vm_alloc(int size) {
     void *mem;
     if (posix_memalign(&mem, 4096, size) < 0) {
          fprintf(stderr, "Allocation failed\n");
          exit(2);
     }
     return mem;
}
