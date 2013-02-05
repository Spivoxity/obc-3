#include "vm.h"
#include <stdlib.h>
#include <stdio.h>

typedef int (*funcp)(int);

funcp compile(void) {
     code_addr entry, lab1, lab2, loc1;
     int arg;

     entry = vm_begin("fact", 1);
     arg = vm_arg();
     vm_gen2ri(GETARG, xV0, arg);
     vm_gen2ri(MOV, xV1, 1);

     lab1 = vm_label();
     loc1 = vm_gen3rib(BEQ, xV0, 0, NULL);
     vm_gen3rrr(MUL, xV1, xV1, xV0);
     vm_gen3rri(SUB, xV0, xV0, 1);
     vm_gen1b(JUMP, lab1);

     lab2 = vm_label();
     vm_gen2rr(MOV, xRET, xV1);
     vm_gen0(RET);

     vm_patch(loc1, lab2);
     vm_end();

     return (funcp) entry;
}

funcp compile2(void) {
     code_addr entry, lab1, lab2, loc1, loc2;
     int arg;

     entry = vm_begin("fact", 1);
     arg = vm_arg();
     vm_gen2ri(GETARG, xV0, arg);

     loc1 = vm_gen3rib(BNEQ, xV0, 0, NULL);
     vm_gen2ri(MOV, xRET, 1);
     loc2 = vm_gen1b(JUMP, NULL);
     
     lab1 = vm_label();
     vm_gen3rri(SUB, xR0, xV0, 1);
     vm_gen1i(PREP, 1);
     vm_gen1r(ARG, xR0);
     vm_gen1i(CALL, (int) entry);
     vm_gen1r(RETVAL, xR1);
     vm_gen3rrr(MUL, xRET, xV0, xR1);

     lab2 = vm_label();
     vm_gen0(RET);

     vm_patch(loc1, lab1);
     vm_patch(loc2, lab2);
     vm_end();

     return (funcp) entry;
}

int main(int argc, char *argv[]) {
     funcp fp;
     int n;

     vm_debug = 2;
     fp = compile2();
     n = (argc > 1 ? atoi(argv[1]) : 10);
     printf("The factorial of %d is %d\n", n, (*fp)(n));
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
