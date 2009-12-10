/* $Id$ */

/* Some helpers for en/decoding XDR faster */

#include "netsys_c.h"
#include <arpa/inet.h>

CAMLprim value netsys_s_read_int4_64(value sv, value pv) 
{
    char *s;
    long p;

    s = String_val(sv);
    p = Long_val(pv);
    return Val_long((long) (ntohl (*((unsigned int *) (s+p)))));
}

CAMLprim value netsys_s_write_int4_64(value sv, value pv, value iv) 
{
    char *s;
    long p;

    s = String_val(sv);
    p = Long_val(pv);
    *((unsigned int *) (s+p)) = htonl ((unsigned int) Long_val(iv));
    return Val_unit;
}

