/* $Id$
 * ----------------------------------------------------------------------
 */

/* Linux: make all system prototypes available */
#define _GNU_SOURCE

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

#ifdef HAVE_POSIX_SHM
#include <sys/types.h>
#include <sys/mman.h>
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
