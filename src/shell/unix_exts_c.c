/* $Id: unix_exts_c.c,v 1.1 2000/12/10 15:12:54 gerd Exp $
 * ----------------------------------------------------------------------
 *
 */

/* Linux: make all system prototypes available */
#define _GNU_SOURCE

#include "caml/mlvalues.h"
#include "caml/alloc.h"
#include <unistd.h>
#include <stdio.h>
#include <fcntl.h>
#include <errno.h>
#include <signal.h>

/**********************************************************************/
/* From unixsupport.h                                                 */
/**********************************************************************/

#define Nothing ((value) 0)

extern void unix_error (int errcode, char * cmdname, value arg) Noreturn;
extern void uerror (char * cmdname, value arg) Noreturn;

/**********************************************************************/

value unix__exit (value n) {
    _exit(Int_val(n));
    return Val_int(0);
}


value unix_sysconf_open_max (value unit) {
    return Val_long(sysconf(_SC_OPEN_MAX));
}


value unix_getpgid (value pid) {
    int pgid;

    pgid = getpgid(Int_val(pid));
    if (pgid == -1) uerror("getpgid", Nothing);
    return Val_int(pgid);
}


value unix_setpgid (value pid, value pgid) {
    int r;

    r = setpgid(Int_val(pid), Int_val(pgid));
    if (r == -1) uerror("setpgid", Nothing);
    return Val_int(0);
}


value unix_tcgetpgrp (value fd) {
    int pgid;

    pgid = tcgetpgrp(Int_val(fd));
    if (pgid == -1) uerror("tcgetpgrp", Nothing);
    return Val_int(pgid);
}


value unix_tcsetpgrp (value fd, value pgid) {
    int r;
    
    r = tcsetpgrp(Int_val(fd), Int_val(pgid));
    if (r == -1) uerror("tcsetpgrp", Nothing);
    return Val_int(0);
}


value unix_ctermid (value unit) {
    return copy_string(ctermid(NULL));
    /* ctermid is always successful; however it can return an empty string */
}


value unix_ttyname (value fd) {
    char *s;

    s = ttyname(Int_val(fd));
    if ( s == NULL ) uerror("ttyname", Nothing);
    return copy_string(s);
}


value unix_getsid (value pid) {
    int sid;

    sid = getsid(Int_val(pid));
    if ( sid == -1 )  uerror("getsid", Nothing);
    return Val_int(sid);
}


value unix_setreuid(value ruid, value euid) {
    int r;

    r = setreuid(Int_val(ruid), Int_val(euid));
    if (r == -1) uerror("setreuid", Nothing);
    return Val_int(0);
}


value unix_setregid(value rgid, value egid) {
    int r;

    r = setregid(Int_val(rgid), Int_val(egid));
    if (r == -1) uerror("setregid", Nothing);
    return Val_int(0);
}

/* ======================================================================
 * History:
 * 
 * $Log: unix_exts_c.c,v $
 * Revision 1.1  2000/12/10 15:12:54  gerd
 * 	Initial revision.
 *
 * 
 */
