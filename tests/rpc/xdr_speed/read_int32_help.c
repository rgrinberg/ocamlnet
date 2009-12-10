/* C helpers */

#include "caml/mlvalues.h"
#include "caml/alloc.h"
#include <arpa/inet.h>

/* decodes the 32 bit int at s[p], converts from network byte order,
   and puts the number into the _already allocated_ nativeint n
*/

value decode_nativeint(value sv, value pv) 
{
    char *s;
    long p;

    s = String_val(sv);
    p = Long_val(pv);
    return caml_copy_nativeint((intnat) (ntohl (*((unsigned int *) (s+p)))));
}
