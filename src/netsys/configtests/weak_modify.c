/* This checks whether CAMLweakdef is non-empty */

#include <string.h>
#include <stdio.h>
#include <caml/config.h>
#include <caml/mlvalues.h>
#include <caml/misc.h>

#define xstr(s) str(s)
#define str(s) #s

value check(value dummy) {
    /* fprintf(stderr,"s=%s\n", xstr(CAMLweakdef)); */
    return (strcmp(xstr(CAMLweakdef), "") != 0) ? 0 : 1;
}

