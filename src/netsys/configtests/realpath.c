#define _GNU_SOURCE

#include <unistd.h>
#include <stdlib.h>
#include <limits.h>

#include "caml/mlvalues.h"
#include "caml/alloc.h"

/* reminder: we return the exit code, and 0 means success */

value check(value dummy) {
    realpath("/", NULL);
    return Val_int(0);
}
