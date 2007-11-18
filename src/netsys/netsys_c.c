/* $Id$
 * ----------------------------------------------------------------------
 */

/* Linux: make all system prototypes available */
#define _GNU_SOURCE
#define _XOPEN_SOURCE 600

#include "config.h"

#ifdef _WIN32
#include "config_win32.h"
#else
#include "config_posix.h"
#include <unistd.h>
#include <stdio.h>
#include <fcntl.h>
#include <errno.h>
#include <signal.h>
#endif

#include "caml/mlvalues.h"
#include "caml/alloc.h"
#include "caml/memory.h"
#include "caml/fail.h"
#include "caml/signals.h"

#ifdef HAVE_POSIX_SHM
#include <sys/types.h>
#include <sys/mman.h>
#endif

#ifdef HAVE_POSIX_FADVISE
#include <sys/types.h>
#include <sys/fcntl.h>
#endif

#ifdef HAVE_POLL
#include <sys/poll.h>
#endif

#include <string.h>

/**********************************************************************/
/* From unixsupport.h                                                 */
/**********************************************************************/

#define Nothing ((value) 0)

extern void unix_error (int errcode, char * cmdname, value arg) Noreturn;
extern void uerror (char * cmdname, value arg) Noreturn;

/**********************************************************************/
/* Standard POSIX stuff                                               */
/**********************************************************************/

value netsys__exit (value n) {
#ifdef HAVE__EXIT
    _exit(Int_val(n));
    return Val_int(0);
#else
    invalid_argument("Netsys._exit not available");
#endif
}


value netsys_sysconf_open_max (value unit) {
#ifdef HAVE_SYSCONF
    return Val_long(sysconf(_SC_OPEN_MAX));
#else
    invalid_argument("Netsys.sysconf_open_max not available");
#endif
}


value netsys_getpgid (value pid) {
#ifdef HAVE_GETPGID
    int pgid;

    pgid = getpgid(Int_val(pid));
    if (pgid == -1) uerror("getpgid", Nothing);
    return Val_int(pgid);
#else
    invalid_argument("Netsys.getpgid not available");
#endif
}


value netsys_setpgid (value pid, value pgid) {
#ifdef HAVE_SETPGID
    int r;

    r = setpgid(Int_val(pid), Int_val(pgid));
    if (r == -1) uerror("setpgid", Nothing);
    return Val_int(0);
#else
    invalid_argument("Netsys.setpgid not available");
#endif
}


value netsys_tcgetpgrp (value fd) {
#ifdef HAVE_TCGETPGRP
    int pgid;

    pgid = tcgetpgrp(Int_val(fd));
    if (pgid == -1) uerror("tcgetpgrp", Nothing);
    return Val_int(pgid);
#else
    invalid_argument("Netsys.tcgetpgrp not available");
#endif
}


value netsys_tcsetpgrp (value fd, value pgid) {
#ifdef HAVE_TCSETPGRP
    int r;
    
    r = tcsetpgrp(Int_val(fd), Int_val(pgid));
    if (r == -1) uerror("tcsetpgrp", Nothing);
    return Val_int(0);
#else
    invalid_argument("Netsys.tcsetpgrp not available");
#endif
}


value netsys_ctermid (value unit) {
#ifdef HAVE_CTERMID
    char *s;
    s = NULL;
    return copy_string(ctermid(s));
    /* ctermid is always successful; however it can return an empty string */
#else
    invalid_argument("Netsys.ctermid not available");
#endif
}


value netsys_ttyname (value fd) {
#ifdef HAVE_TTYNAME
    char *s;

    s = ttyname(Int_val(fd));
    if ( s == NULL ) uerror("ttyname", Nothing);
    return copy_string(s);
#else
    invalid_argument("Netsys.ttyname not available");
#endif
}


value netsys_getsid (value pid) {
#ifdef HAVE_GETSID
    int sid;

    sid = getsid(Int_val(pid));
    if ( sid == -1 )  uerror("getsid", Nothing);
    return Val_int(sid);
#else
    invalid_argument("Netsys.getsid not available");
#endif
}


value netsys_setreuid(value ruid, value euid) {
#ifdef HAVE_SETREUID
    int r;

    r = setreuid(Int_val(ruid), Int_val(euid));
    if (r == -1) uerror("setreuid", Nothing);
    return Val_int(0);
#else
    invalid_argument("Netsys.setreuid not available");
#endif
}


value netsys_setregid(value rgid, value egid) {
#ifdef HAVE_SETREGID
    int r;

    r = setregid(Int_val(rgid), Int_val(egid));
    if (r == -1) uerror("setregid", Nothing);
    return Val_int(0);
#else
    invalid_argument("Netsys.setregid not available");
#endif
}


value netsys_fsync(value fd) {
#ifdef HAVE_FSYNC
    int r;
    r = fsync(Int_val(fd));
    if (r == -1) 
	uerror("fsync", Nothing);
    return Val_unit;
#else
    invalid_argument("Netsys.fsync not available");
#endif
}


value netsys_fdatasync(value fd) {
#ifdef HAVE_FDATASYNC
    int r;
    r = fdatasync(Int_val(fd));
    if (r == -1) 
	uerror("fdatasync", Nothing);
    return Val_unit;
#else
    invalid_argument("Netsys.fdatasync not available");
#endif
}

/**********************************************************************/
/* poll interface                                                     */
/**********************************************************************/

value netsys_pollfd_size (value dummy) {
#ifdef HAVE_POLL
    return Val_int(sizeof(struct pollfd));
#else
    return Val_int(0);
#endif
}


value netsys_mk_string_poll_cell(value fd, value ev, value rev) {
#ifdef HAVE_POLL
    value s;
    struct pollfd p;
    p.fd = Int_val(fd);
    p.events = Int_val(ev);
    p.revents = Int_val(rev);
    s = caml_alloc_string(sizeof(struct pollfd));
    memcpy(String_val(s), &p, sizeof(struct pollfd));
    return s;
#else
    invalid_arg("netsys_mk_string_poll_cell");
#endif
}


value netsys_rd_string_poll_cell(value s) {
#ifdef HAVE_POLL
    struct pollfd p;
    value triple;
    memcpy(&p, String_val(s),  sizeof(struct pollfd));
    triple = caml_alloc_tuple(3);
    Store_field(triple, 0, Val_int(p.fd));
    Store_field(triple, 1, Val_int(p.events));
    Store_field(triple, 2, Val_int(p.revents));
    return triple;
#else
    invalid_arg("netsys_rd_string_poll_cell");
#endif
}

#ifdef HAVE_POLL
#define CONST_POLLIN POLLIN
#define CONST_POLLPRI POLLPRI
#define CONST_POLLOUT POLLOUT
#define CONST_POLLERR POLLERR
#define CONST_POLLHUP POLLHUP
#define CONST_POLLNVAL POLLNVAL
#else
#define CONST_POLLIN 0x1
#define CONST_POLLPRI 0x2
#define CONST_POLLOUT 0x4
#define CONST_POLLERR 0x8
#define CONST_POLLHUP 0x10
#define CONST_POLLNVAL 0x20
#endif


value netsys_poll_constants(value dummy) {
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


value netsys_poll(value av, value nv, value tv) {
#ifdef HAVE_POLL
    struct pollfd *a;
    struct pollfd  copy100[100];
    struct pollfd *copy;
    int n;
    double t;
    int tmo, r;

    a = (struct pollfd *) String_val(av);
    /* Note that the string is always word-aligned, so creating a pointer
       is no problem
    */
    
    n = Int_val(nv);
    t = Double_val(tv);
    if (t < 0)
	tmo = (-1);
    else
	tmo = t * 1000;
    
    /* Unfortunately, we have to copy [a] to non-caml memory. Otherwise
       multi-threading would not work
    */
    if (n <= 100) {
	memcpy(copy100, a, n*sizeof(struct pollfd));
	copy = NULL;
    } else {
	copy = caml_stat_alloc(n*sizeof(struct pollfd));
	memcpy(copy, a, n*sizeof(struct pollfd));
    };

    enter_blocking_section();
    r = poll( (copy != NULL ? copy : copy100), n, tmo);
    leave_blocking_section();

    if (copy != NULL)
	caml_stat_free(copy);

    if (r == -1) uerror("poll", Nothing);
    
    return Val_int(r);
#else
     invalid_arg("netsys_poll");
#endif
}


/**********************************************************************/
/* POSIX fadvise                                                      */
/**********************************************************************/

/* A lately added POSIX function */

value netsys_have_posix_fadvise(value dummy) {
#ifdef HAVE_POSIX_FADVISE
    return Val_bool(1);
#else
    return Val_bool(0);
#endif
}

value netsys_fadvise(value fd, value start, value len, value adv) {
#ifdef HAVE_POSIX_FADVISE
    int adv_int, r;
    long long start_int, len_int;

    adv_int = 0;
    switch (Int_val(adv)) {
    case 0: adv_int = POSIX_FADV_NORMAL; break;
    case 1: adv_int = POSIX_FADV_SEQUENTIAL; break;
    case 2: adv_int = POSIX_FADV_RANDOM; break;
    case 3: adv_int = POSIX_FADV_NOREUSE; break;
    case 4: adv_int = POSIX_FADV_WILLNEED; break;
    case 5: adv_int = POSIX_FADV_DONTNEED; break;
    default: invalid_argument("Netsys.fadvise");
    };

    start_int = Int64_val(start);
    len_int = Int64_val(len);
    r = posix_fadvise64(Int_val(fd), start_int, len_int, adv_int);
    if (r == -1) 
	uerror("posix_fadvise64", Nothing);
    return Val_unit;
#else
    invalid_argument("Netsys.fadvise not available");
#endif
}

/**********************************************************************/
/* POSIX fallocate                                                    */
/**********************************************************************/

/* A lately added POSIX function */

value netsys_have_posix_fallocate(value dummy) {
#ifdef HAVE_POSIX_FALLOCATE
    return Val_bool(1);
#else
    return Val_bool(0);
#endif
}


value netsys_fallocate(value fd, value start, value len) {
#ifdef HAVE_POSIX_FALLOCATE
    int r;
    long long start_int, len_int;
    start_int = Int64_val(start);
    len_int = Int64_val(len);
    r = posix_fallocate64(Int_val(fd), start_int, len_int);
    /* does not set errno! */
    if (r != 0) 
	unix_error(r, "posix_fallocate64", Nothing);
    return Val_unit;
#else
    invalid_argument("Netsys.fallocate not available");
#endif
}

/**********************************************************************/
/* POSIX shared memory                                                */
/**********************************************************************/

/* This is from the POSIX realtime extensions. Not every POSIX-type OS
 * supports it.
 */

value netsys_have_posix_shm(value dummy) {
#ifdef HAVE_POSIX_SHM
    return Val_bool(1);
#else
    return Val_bool(0);
#endif
}

#ifdef HAVE_POSIX_SHM
static int shm_open_flag_table[] = {
    O_RDONLY, O_RDWR, O_CREAT, O_EXCL, O_TRUNC
};
#endif


value netsys_shm_open(value path, value flags, value perm)
{
#ifdef HAVE_POSIX_SHM
    CAMLparam3(path, flags, perm);
    int ret, cv_flags;
    char * p;

    cv_flags = convert_flag_list(flags, shm_open_flag_table);
    p = stat_alloc(string_length(path) + 1);
    strcpy(p, String_val(path));
    ret = shm_open(p, cv_flags, Int_val(perm));
    stat_free(p);
    if (ret == -1) uerror("shm_open", path);
    CAMLreturn (Val_int(ret));
#else
    invalid_argument("Netsys.shm_open not available");
#endif
}


value netsys_shm_unlink(value path)
{
#ifdef HAVE_POSIX_SHM
    int ret;

    ret = shm_unlink(String_val(path));
    if (ret == -1) uerror("shm_unlink", path);
    return Val_unit;
#else
    invalid_argument("Netsys.shm_unlink not available");
#endif
}

/**********************************************************************/
/* I/O priorities (Linux-only)                                        */
/**********************************************************************/

/* Available since kernel 2.6.13 */

/* There is no glibc support for these calls. 
   See http://sourceware.org/bugzilla/show_bug.cgi?id=4464
   for Drepper's opinion about that.
*/

#ifdef __linux__

#if defined(__i386__)
#define __NR_ioprio_set         289
#define __NR_ioprio_get         290
#define ioprio_supported
#elif defined(__ppc__)
#define __NR_ioprio_set         273
#define __NR_ioprio_get         274
#define ioprio_supported
#elif defined(__x86_64__)
#define __NR_ioprio_set         251
#define __NR_ioprio_get         252
#define ioprio_supported
#elif defined(__ia64__)
#define __NR_ioprio_set         1274
#define __NR_ioprio_get         1275
#define ioprio_supported
/* Other architectures unsupported */
#endif

#endif
/* __linux__ */


#ifdef ioprio_supported

static inline int ioprio_set (int which, int who, int ioprio)
{
    return syscall (__NR_ioprio_set, which, who, ioprio);
}

static inline int ioprio_get (int which, int who)
{
        return syscall (__NR_ioprio_get, which, who);
}

enum {
        IOPRIO_CLASS_NONE,
        IOPRIO_CLASS_RT,
        IOPRIO_CLASS_BE,
        IOPRIO_CLASS_IDLE,
};

enum {
        IOPRIO_WHO_PROCESS = 1,
        IOPRIO_WHO_PGRP,
        IOPRIO_WHO_USER,
};

#define IOPRIO_CLASS_SHIFT      13
#define IOPRIO_PRIO_MASK        0xff

#endif
/* ioprio_supported */


value netsys_ioprio_get(value target) {
#ifdef ioprio_supported
    int ioprio;
    int ioprio_class;
    int ioprio_data;
    value result;

    switch (Tag_val(target)) {
    case 0:
	ioprio = ioprio_get(IOPRIO_WHO_PROCESS, Int_val(Field(target, 0)));
	break;
    case 1:
	ioprio = ioprio_get(IOPRIO_WHO_PGRP, Int_val(Field(target, 0)));
	break;
    case 2:
	ioprio = ioprio_get(IOPRIO_WHO_USER, Int_val(Field(target, 0)));
	break;
    default:
	failwith("netsys_ioprio_get: internal error");
    }

    if (ioprio == -1)
	uerror("ioprio_get", Nothing);

    ioprio_class = ioprio >> IOPRIO_CLASS_SHIFT;
    ioprio_data = ioprio & IOPRIO_PRIO_MASK;

    switch (ioprio_class) {
    case IOPRIO_CLASS_NONE:
	result = Val_long(0);
	break;
    case IOPRIO_CLASS_RT:
	result = caml_alloc(1, 0);
	Store_field(result, 0, Val_int(ioprio_data));
	break;
    case IOPRIO_CLASS_BE:
	result = caml_alloc(1, 1);
	Store_field(result, 0, Val_int(ioprio_data));
	break;
    case IOPRIO_CLASS_IDLE:
	result = Val_long(1);
	break;
    default:
	failwith("netsys_ioprio_get: Unexpected result");
    }
    
    return result;

#else
    /* not ioprio_supported: */
    unix_error(ENOSYS, "ioprio_get", Nothing);
#endif
    /* ioprio_supported */
}


value netsys_ioprio_set(value target, value ioprio_arg) {
#ifdef ioprio_supported
    int ioprio;
    int ioprio_class;
    int ioprio_data;
    int sysres;

    if (Is_block(ioprio_arg)) {
	switch (Tag_val(ioprio_arg)) {
	case 0:
	    ioprio_class = IOPRIO_CLASS_RT;
	    ioprio_data = Int_val(Field(ioprio_arg, 0));
	    break;
	case 1:
	    ioprio_class = IOPRIO_CLASS_BE;
	    ioprio_data = Int_val(Field(ioprio_arg, 0));
	    break;
	default:
	    failwith("netsys_ioprio_set: internal error");
	}
    } else {
	switch (Long_val(ioprio_arg)) {
	case 0:
	    /* Makes no sense. We behave in the same way as ionice */
	    ioprio_class = IOPRIO_CLASS_BE;
	    ioprio_data = 4;
	    break;
	case 1:
	    ioprio_class = IOPRIO_CLASS_IDLE;
	    ioprio_data = 7;
	    break;
	default:
	    failwith("netsys_ioprio_set: internal error");
	}
    };

    ioprio = (ioprio_class << IOPRIO_CLASS_SHIFT) | (ioprio_data & IOPRIO_PRIO_MASK);

    switch (Tag_val(target)) {
    case 0:
	sysres = ioprio_set(IOPRIO_WHO_PROCESS, Int_val(Field(target, 0)), ioprio);
	break;
    case 1:
	sysres = ioprio_set(IOPRIO_WHO_PGRP, Int_val(Field(target, 0)), ioprio);
	break;
    case 2:
	sysres = ioprio_set(IOPRIO_WHO_USER, Int_val(Field(target, 0)), ioprio);
	break;
    default:
	failwith("netsys_ioprio_set: internal error");
    }

    if (sysres == -1)
	uerror("ioprio_set", Nothing);

    return Val_unit;
#else
    /* not ioprio_supported: */
    unix_error(ENOSYS, "ioprio_set", Nothing);
#endif
    /* ioprio_supported */
}
