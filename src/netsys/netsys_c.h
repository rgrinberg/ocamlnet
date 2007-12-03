/* $Id$ */

#include "config.h"

#ifdef _WIN32
#include "config_win32.h"

#define WIN32_LEAN_AND_MEAN
#include <wtypes.h>
#include <winbase.h>
#include <stdlib.h>
#include <winsock2.h>
#include <ws2tcpip.h>
#include <errno.h>

#else

#include "config_posix.h"
#include <unistd.h>
#include <stdio.h>
#include <fcntl.h>
#include <errno.h>
#include <signal.h>

#endif

#include <string.h>

#include "caml/mlvalues.h"
#include "caml/alloc.h"
#include "caml/memory.h"
#include "caml/fail.h"
#include "caml/signals.h"
#include "caml/custom.h"


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

/**********************************************************************/
/* From unixsupport.h                                                 */
/**********************************************************************/

#define Nothing ((value) 0)

extern void unix_error (int errcode, char * cmdname, value arg) Noreturn;
extern void uerror (char * cmdname, value arg) Noreturn;

#ifdef _WIN32
extern void win32_maperr(DWORD errcode);

struct filedescr {
  union {
    HANDLE handle;
    SOCKET socket;
  } fd;
  enum { KIND_HANDLE, KIND_SOCKET } kind;
  int crt_fd;
};

#define Handle_val(v) (((struct filedescr *) Data_custom_val(v))->fd.handle)
#define Socket_val(v) (((struct filedescr *) Data_custom_val(v))->fd.socket)
#define Descr_kind_val(v) (((struct filedescr *) Data_custom_val(v))->kind)
#define CRT_fd_val(v) (((struct filedescr *) Data_custom_val(v))->crt_fd)

extern value win_alloc_handle_or_socket(HANDLE);
extern value win_alloc_handle(HANDLE);
extern value win_alloc_socket(SOCKET);

#endif
