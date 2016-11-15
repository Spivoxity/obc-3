#include <unistd.h>
#include "obx.h"

PRIMDEF void Sleep_Usec(value *bp) {
     usleep(bp[HEAD+0].i);
     ob_res.i = 42;
}
