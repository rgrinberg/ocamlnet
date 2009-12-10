/* C helpers */

#include "caml/mlvalues.h"

/* decodes the 32 bit int at s[p], converts from network byte order,
   and puts the number into the _already allocated_ nativeint n
*/

value decode_nativeint(value sv, value pv, value nv) 
{
    char *s;
    long p;

    s = String_val(sv);
    p = Long_val(pv);
    Nativeint_val(nv) = *((intnat *) (s+p));

    return Val_unit;
}
