#ifdef __linux__
#define _GNU_SOURCE
#define _XOPEN_SOURCE 600
#endif

#include <errno.h>
#include <unistd.h>
#include <syslog.h>

#include "caml/mlvalues.h"
#include "caml/alloc.h"

/* reminder: we return the exit code, and 0 means success */

value check(value dummy) {
    openlog(NULL, 0, 0);
    return Val_int(0);
}
