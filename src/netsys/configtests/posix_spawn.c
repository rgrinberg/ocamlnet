#define _GNU_SOURCE

#include <unistd.h>
#include <stdlib.h>
#include <spawn.h>

#include "caml/mlvalues.h"
#include "caml/alloc.h"

/* reminder: we return the exit code, and 0 means success */

value check(value dummy) {
    pid_t pid;
    int code;
    char *argv[2];

    argv[0] = "true";
    argv[1] = NULL;
    code = posix_spawn(&pid, "true", NULL, NULL, argv, environ);
    if (code != 0) return Val_int(1);

    code = waitpid(pid, NULL, 0);
    if (code == -1) return Val_int(1);

    return Val_int(0);
}
