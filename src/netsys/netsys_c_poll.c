/* $Id$ */


#include "netsys_c.h"

#ifdef HAVE_POLL
#include <sys/poll.h>
#endif

/**********************************************************************/
/* poll interface                                                     */
/**********************************************************************/

CAMLprim value netsys_pollfd_size (value dummy) {
#ifdef HAVE_POLL
    return Val_int(sizeof(struct pollfd));
#else
    return Val_int(0);
#endif
}


#define Poll_mem_val(v) ((struct pollfd **) (Data_custom_val(v)))

#ifdef HAVE_POLL
static void finalize_poll_mem(value r) {
    caml_stat_free(*(Poll_mem_val(r)));
}

static struct custom_operations poll_mem_ops = {
    "",
    finalize_poll_mem,
    custom_compare_default,
    custom_hash_default,
    custom_serialize_default,
    custom_deserialize_default
};

static value alloc_poll_mem(int n) {
    struct pollfd *p;
    value r;
    p = caml_stat_alloc(n * sizeof(struct pollfd));
    r = caml_alloc_custom(&poll_mem_ops, sizeof(p), n, 100000);
    *(Poll_mem_val(r)) = p;
    return r;
};
#endif


CAMLprim value netsys_mk_poll_mem(value n) {
#ifdef HAVE_POLL
    value s;
    struct pollfd p;
    int k;
    p.fd = 0;
    p.events = 0;
    p.revents = 0;
    s = alloc_poll_mem(n);
    for (k=0; k<n; k++) {
	(*(Poll_mem_val(s)))[k] = p;
    };
    return s;
#else
    invalid_argument("netsys_mk_poll_mem");
#endif
}


CAMLprim value netsys_set_poll_mem(value s, value k, value fd, value ev, value rev) {
#ifdef HAVE_POLL
    struct pollfd p;
    p.fd = Int_val(fd);
    p.events = Int_val(ev);
    p.revents = Int_val(rev);
    (*(Poll_mem_val(s)))[Int_val(k)] = p;
    return Val_unit;
#else
    invalid_argument("netsys_set_poll_mem");
#endif

}


CAMLprim value netsys_get_poll_mem(value s, value k) {
#ifdef HAVE_POLL
    struct pollfd p;
    value triple;
    p = (*(Poll_mem_val(s)))[Int_val(k)];
    triple = caml_alloc_tuple(3);
    Store_field(triple, 0, Val_int(p.fd));
    Store_field(triple, 1, Val_int(p.events));
    Store_field(triple, 2, Val_int(p.revents));
    return triple;
#else
    invalid_argument("netsys_get_poll_mem");
#endif
}


CAMLprim value netsys_blit_poll_mem(value s1, value k1, value s2, value k2, value l) {
#ifdef HAVE_POLL
    struct pollfd *p1;
    struct pollfd *p2;
    p1 = *(Poll_mem_val(s1));
    p2 = *(Poll_mem_val(s2));
    memmove(p2 + Int_val(k2), p1 + Int_val(k1), l*sizeof(struct pollfd));
    return Val_unit;
#else
    invalid_argument("netsys_blit_poll_mem");
#endif
};


CAMLprim value netsys_poll_constants(value dummy) {
    value r;
    r = caml_alloc_tuple(6);
    Store_field(r, 0, Val_int(CONST_POLLIN));
    Store_field(r, 1, Val_int(CONST_POLLPRI));
    Store_field(r, 2, Val_int(CONST_POLLOUT));
    Store_field(r, 3, Val_int(CONST_POLLERR));
    Store_field(r, 4, Val_int(CONST_POLLHUP));
    Store_field(r, 5, Val_int(CONST_POLLNVAL));
    return r;
}


CAMLprim value netsys_poll(value s, value nv, value tv) {
#ifdef HAVE_POLL
    struct pollfd *p;
    int n;
    long tmo, r;

    p = (*(Poll_mem_val(s)));
    n = Int_val(nv);
    tmo = Long_val(tv);
    
    enter_blocking_section();
    r = poll(p, n, tmo);
    leave_blocking_section();

    if (r == -1) uerror("poll", Nothing);
    
    return Val_int(r);
#else
     invalid_argument("netsys_poll");
#endif
}
