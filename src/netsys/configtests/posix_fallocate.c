#define _XOPEN_SOURCE 600

#include <fcntl.h>
#include <sys/types.h>
#include <sys/fcntl.h>
#include <errno.h>
#include <unistd.h>

#include "caml/mlvalues.h"
#include "caml/alloc.h"

value check(value dummy) {
    int j, k;

    j = open("posix_fallocate.c", O_RDONLY, 0);
    if (j == -1) return Val_int(0);   /* strange */

    k = posix_fallocate64(j, 0, 0);
    if (j != EBADF) return Val_int(0);

    return Val_int(1);
}
