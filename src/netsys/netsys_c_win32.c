/* $Id$ */

/* Note: need -lws2_32 to link this file, e.g. */
/* ocamlmktop -o otop -I . -cclib -lws2_32 unix.cma netsys.cma */

#include "netsys_c.h"
#include <stdio.h>
#include <assert.h>

#ifdef _WIN32

#include "unixsupport_w32.c"
#include <wincrypt.h>
#include <stdarg.h>

static int debug = 1;


static void dprintf(const char *fmt, ...) {
    va_list list;
    if (debug) {
	va_start(list, fmt);
	vfprintf(stderr, fmt, list);
	va_end(list);
	fflush(stderr);
    }
}


static HCRYPTPROV crypt_provider;
static int        crypt_provider_init = 0;
#endif


CAMLprim value netsys_fill_random (value s) {
#ifdef _WIN32
    if (!crypt_provider_init) {
	if (!CryptAcquireContext(&crypt_provider,
				 NULL,
				 NULL,
				 PROV_RSA_FULL,
				 CRYPT_VERIFYCONTEXT)) {
	    win32_maperr(GetLastError());
	    uerror("CryptAcquireContext", Nothing);
	};
	crypt_provider_init = 1;
    }
    if(!CryptGenRandom(crypt_provider,
		       string_length(s),
		       String_val(s))) {
	win32_maperr(GetLastError());
	uerror("CryptGenRandom", Nothing);
    };
    return Val_unit;
#else
    invalid_argument("netsys_fill_random");
#endif
}


#ifdef _WIN32
/* Additions to the socket/handle API for Win32: */

static struct custom_operations event_ops = {
    "",
    custom_finalize_default,
    custom_compare_default,
    custom_hash_default,
    custom_serialize_default,
    custom_deserialize_default
};


struct event {
    HANDLE ev;
    int    mask;
};



#define event_val(v) ((struct event *) (Data_custom_val(v)))

#endif


CAMLprim value netsys_create_event(value dummy) {
#ifdef _WIN32
    HANDLE e;
    value r;
    struct event e0;

    e = CreateEvent(NULL, 1, 0, NULL);
    if (e == NULL) {
	win32_maperr(GetLastError());
	uerror("CreateEvent", Nothing);
    }
    e0.ev = e;
    e0.mask = 0;

    r = caml_alloc_custom(&event_ops, sizeof(struct event), 1, 0);
    *(event_val(r)) = e0;

    return r;
#else
    invalid_argument("netsys_create_event");
#endif
}

CAMLprim value netsys_close_event(value ev) {
#ifdef _WIN32
    struct event e;

    e = *(event_val(ev));
    if (!CloseHandle(e.ev)) {
	/* ignore errors - this is a finaliser! */
    }

    return Val_unit;
#else
    invalid_argument("netsys_close_event");
#endif
}


CAMLprim value netsys_set_event(value ev) {
#ifdef _WIN32
    struct event e;

    e = *(event_val(ev));
    if (!SetEvent(e.ev)) {
	win32_maperr(GetLastError());
	uerror("SetEvent", Nothing);
    }

    return Val_unit;
#else
    invalid_argument("netsys_set_event");
#endif
}

CAMLprim value netsys_reset_event(value ev) {
#ifdef _WIN32
    struct event e;

    e = *(event_val(ev));
    if (!ResetEvent(e.ev)) {
	win32_maperr(GetLastError());
	uerror("ResetEvent", Nothing);
    }

    return Val_unit;
#else
    invalid_argument("netsys_reset_event");
#endif
}


CAMLprim value netsys_test_event(value ev) {
#ifdef _WIN32
    struct event e;
    DWORD n;
    int signaled;

    e = *(event_val(ev));
    n = WaitForSingleObject(e.ev, 0);
    if (n == WAIT_FAILED) {
	win32_maperr(GetLastError());
	uerror("WaitForSingleObject", Nothing);
    };

    return Val_bool(n == WAIT_OBJECT_0);
#else
    invalid_argument("netsys_test_event");
#endif
}


CAMLprim value netsys_event_wait(value ev, value tmo) {
#ifdef _WIN32
    struct event e;
    DWORD n;
    DWORD wtmo;  /* unsigned! */
    int signaled;

    wtmo = INFINITE;
    if (Long_val(tmo) >= 0) {
	wtmo = Long_val(tmo);
    };

    e = *(event_val(ev));
    n = WaitForSingleObject(e.ev, wtmo);
    if (n == WAIT_FAILED) {
	win32_maperr(GetLastError());
	uerror("WaitForSingleObject", Nothing);
    };

    return Val_bool(n == WAIT_OBJECT_0);
#else
    invalid_argument("netsys_event_wait");
#endif
}


CAMLprim value netsys_event_descr(value ev) {
#ifdef _WIN32
    struct event e;
    e = *(event_val(ev));
    return netsysw32_win_alloc_handle(e.ev);
#else
    invalid_argument("netsys_pipe_descr");
#endif
}


CAMLprim value netsys_wsa_event_select(value ev, value fdv, value evmaskv) {
#ifdef _WIN32
    struct event e;
    SOCKET s;
    int m;
    long m_win32;

    e = *(event_val(ev));
    s = Socket_val(fdv);
    m = Int_val(evmaskv);

    e.mask = m & (CONST_POLLIN | CONST_POLLOUT | CONST_POLLPRI);
    *(event_val(ev)) = e;

    m_win32 = 0;
    if ((m & CONST_POLLIN) != 0)  m_win32 |= FD_READ | FD_ACCEPT | FD_CLOSE;
    if ((m & CONST_POLLOUT) != 0) m_win32 |= FD_WRITE | FD_CONNECT | FD_CLOSE;
    if ((m & CONST_POLLPRI) != 0) m_win32 |= FD_OOB;

    if (WSAEventSelect(s, e.ev, m_win32) != 0) {
	win32_maperr(WSAGetLastError());
	uerror("WSAEventSelect", Nothing);
    }

    return Val_unit;
#else
    invalid_argument("netsys_wsa_event_select");
#endif
}

CAMLprim value netsys_wsa_maximum_wait_events(value dummy) {
#ifdef _WIN32
    return Val_int(WSA_MAXIMUM_WAIT_EVENTS);
#else
    invalid_argument("netsys_wsa_maximum_wait_events");
#endif
}

CAMLprim value netsys_wsa_wait_for_multiple_events(value fdarray, value tmov) {
#ifdef _WIN32
    WSAEVENT earray[WSA_MAXIMUM_WAIT_EVENTS];
    struct event e;
    long  tmo0;
    DWORD tmo;   /* note: DWORD is unsigned */
    int k,n;
    DWORD r;
    value rv;

    tmo0 = Long_val(tmov);
    n = Wosize_val(fdarray);

    dprintf("WSAWaitForMultipleEvents prep n=%d tmo=%d\n", n, tmo0);

    for (k=0; k < n; k++) {
	e = *(event_val(Field(fdarray,k)));
	earray[k] = e.ev;
	dprintf("WSAWaitForMultipleEvents prep ev=%u\n", e.ev);
    }

    if (n == 0) {
	if (tmo >= 0) 
	    tmo = tmo0;
	else
	    tmo = INFINITE;
	enter_blocking_section();
	r = SleepEx(tmo, 1);
	leave_blocking_section();
	
	if (r == WAIT_IO_COMPLETION) {
	    win32_maperr(EINTR);
	    uerror("SleepEx", Nothing);
	}

	return Val_int(0);    /* None */
    }
    else {
	if (tmo >= 0) 
	    tmo = tmo0;
	else
	    tmo = INFINITE;
	dprintf("WSAWaitForMultipleEvents start tmo=%u\n", n, tmo);
	enter_blocking_section();
	r = WSAWaitForMultipleEvents(n, earray, 0, tmo, 1);
	leave_blocking_section();
	dprintf("WSAWaitForMultipleEvents end code=%u\n", r);
    
	if (r == WSA_WAIT_FAILED) {
	    win32_maperr(WSAGetLastError());
	    uerror("WSAWaitForMultipleEvents", Nothing);
	}
	
	if (r == WSA_WAIT_TIMEOUT)
	    return Val_int(0);    /* None */
	
	if (r == WSA_WAIT_IO_COMPLETION) {
	    win32_maperr(EINTR);
	    uerror("WSAWaitForMultipleEvents", Nothing);
	}
	
	if (r >= WSA_WAIT_EVENT_0 && r < WSA_WAIT_EVENT_0 + n) {
	    rv = caml_alloc_tuple(1);
	    Store_field(rv, 0, Val_long(r - WSA_WAIT_EVENT_0));
	    return rv;
	}
    }
    
    invalid_argument("netsys_wsa_wait_for_multiple_events: bad return value from Win32");
#else
    invalid_argument("netsys_wsa_wait_for_multiple_events");
#endif
}

CAMLprim value netsys_wsa_enum_network_events(value fdv, value ev) {
#ifdef _WIN32
    struct event e;
    SOCKET s;
    WSANETWORKEVENTS ne;
    int r;

    e = *(event_val(ev));
    s = Socket_val(fdv);

    if (WSAEnumNetworkEvents(s, e.ev, &ne) != 0) {
	win32_maperr(WSAGetLastError());
	uerror("WSAEnumNetworkEvents", Nothing);
    }

    /* printf("NetworkEvents=%ld\n", ne.lNetworkEvents); */

    r = 0;
    if ((ne.lNetworkEvents & FD_CONNECT) != 0) {
	r |= CONST_POLLOUT;
	if (ne.iErrorCode[FD_CONNECT_BIT] != 0) r |= CONST_POLLERR;
    };
    if ((ne.lNetworkEvents & FD_CLOSE) != 0) {
	r |= CONST_POLLIN;
	if (ne.iErrorCode[FD_CLOSE_BIT] != 0) {
	    if (ne.iErrorCode[FD_CLOSE_BIT] == WSAECONNRESET)
		r |= CONST_POLLHUP;
	    else
		r |= CONST_POLLERR;
	}
    };
    if ((ne.lNetworkEvents & FD_ACCEPT) != 0) {
	r |= CONST_POLLIN;
	if (ne.iErrorCode[FD_ACCEPT_BIT] != 0) r |= CONST_POLLERR;
    };
    if ((ne.lNetworkEvents & FD_READ) != 0) {
	r |= CONST_POLLIN;
	if (ne.iErrorCode[FD_READ_BIT] != 0) r |= CONST_POLLERR;
    };
    if ((ne.lNetworkEvents & FD_WRITE) != 0) {
	r |= CONST_POLLOUT;
	if (ne.iErrorCode[FD_WRITE_BIT] != 0) r |= CONST_POLLERR;
    };
    if ((ne.lNetworkEvents & FD_OOB) != 0) {
	r |= CONST_POLLPRI;
	if (ne.iErrorCode[FD_OOB_BIT] != 0) r |= CONST_POLLERR;
    };

    /* printf("r=%d\n", r); */

    r &= (e.mask | CONST_POLLHUP | CONST_POLLERR);

    /* printf("e.mask=%d\n", e.mask); fflush(stdout); */

    return Val_int(r);
#else
    invalid_argument("netsys_wsa_enum_network_events");
#endif
}

/* CHECK:
   - maybe we have to make the FD_CLOSE event last forever (it is not
     level-triggered)
*/

#ifdef _WIN32
#define PIPE_HELPER_BUF_SIZE 1024

#define PIPE_DEAF 0
#define PIPE_LISTENING 1
#define PIPE_CONNECTED 2
#define PIPE_DOWN 3

struct pipe_helper {
    HANDLE pipe_handle;
    int    pipe_is_open;          /* handle is open */
    int    pipe_is_server;
    int    pipe_conn_state;
    int    pipe_error_rd;         /* recorded async error */
    int    pipe_error_wr;         /* recorded async error */
    int    pipe_mode_rd;          /* reads allowed */
    int    pipe_mode_wr;          /* write allowed */
    HANDLE pipe_rd_ev;
    HANDLE pipe_wr_ev;
    LPOVERLAPPED pipe_rd_ovrlp; /* overlappped structure for reads */
    LPOVERLAPPED pipe_wr_ovrlp; /* overlappped structure for writes/connects */
    int    pipe_rd_ovrlp_started; /* an overlapping read has been started */
    int    pipe_wr_ovrlp_started; /* an overlapping write has been started */
    int    pipe_cn_ovrlp_started; /* an overlapping connect has been started */
    char   pipe_rd_buf[PIPE_HELPER_BUF_SIZE];
    int    pipe_rd_buf_size;
    int    pipe_rd_eof;
    char   pipe_wr_buf[PIPE_HELPER_BUF_SIZE];
    int    pipe_wr_buf_size;
    HANDLE pipe_descr;
    HANDLE pipe_signal;
};

#define pipe_helper_ptr_val(v) ((struct pipe_helper **) (Data_custom_val(v)))

static struct custom_operations pipe_helper_ops = {
    "",
    custom_finalize_default,
    custom_compare_default,
    custom_hash_default,
    custom_serialize_default,
    custom_deserialize_default
};


static struct pipe_helper * alloc_pipe_helper (HANDLE h) {
    struct pipe_helper *ph;
    HANDLE rd_ev;
    HANDLE wr_ev;
    HANDLE pd;
    OVERLAPPED *rd_ovrlp;
    OVERLAPPED *wr_ovrlp;

    /* CHECK: error handling */

    rd_ev = CreateEvent(NULL, 1, 0, NULL);
    if (rd_ev == NULL) {
	win32_maperr(GetLastError());
	uerror("CreateEvent", Nothing);
    };

    wr_ev = CreateEvent(NULL, 1, 0, NULL);
    if (wr_ev == NULL) {
	win32_maperr(GetLastError());
	uerror("CreateEvent", Nothing);
    };

    pd = CreateEvent(NULL, 1, 0, NULL);
    if (pd == NULL) {
	win32_maperr(GetLastError());
	uerror("CreateEvent", Nothing);
    };

    rd_ovrlp = stat_alloc(sizeof(OVERLAPPED));
    ZeroMemory(rd_ovrlp, sizeof(OVERLAPPED));
    rd_ovrlp->hEvent = rd_ev;

    wr_ovrlp = stat_alloc(sizeof(OVERLAPPED));
    ZeroMemory(wr_ovrlp, sizeof(OVERLAPPED));
    wr_ovrlp->hEvent = wr_ev;

    ph = stat_alloc(sizeof(struct pipe_helper));
    ph->pipe_handle = h;
    ph->pipe_is_open = 1;
    ph->pipe_is_server = 0;
    ph->pipe_conn_state = PIPE_DEAF;
    ph->pipe_error_rd = 0;
    ph->pipe_error_wr = 0;
    ph->pipe_mode_rd = 0;
    ph->pipe_mode_wr = 0;
    ph->pipe_rd_ev = rd_ev;
    ph->pipe_wr_ev = wr_ev;
    ph->pipe_rd_ovrlp = rd_ovrlp;
    ph->pipe_wr_ovrlp = wr_ovrlp;
    ph->pipe_rd_ovrlp_started = 0;
    ph->pipe_wr_ovrlp_started = 0;
    ph->pipe_cn_ovrlp_started = 0;
    ph->pipe_rd_buf_size = 0;
    ph->pipe_rd_eof = 0;
    ph->pipe_wr_buf_size = 0;
    ph->pipe_signal = NULL;
    ph->pipe_descr = pd;

    return ph;
}


static void free_pipe_helper(struct pipe_helper *ph) {
    if (ph->pipe_is_open)
	CloseHandle(ph->pipe_handle);
    CloseHandle(ph->pipe_rd_ev);
    CloseHandle(ph->pipe_wr_ev);
    CloseHandle(ph->pipe_descr);
    /* do nothing about pipe_signal */
    stat_free(ph->pipe_rd_ovrlp);
    stat_free(ph->pipe_wr_ovrlp);
    stat_free(ph);
}


static void start_reading(struct pipe_helper *ph) {
    int flag;
    DWORD err;
    DWORD n;

    dprintf("PIPE start_reading %u\n",
	    ph->pipe_handle);

    flag = ReadFile(ph->pipe_handle,
		    ph->pipe_rd_buf,
		    PIPE_HELPER_BUF_SIZE,
		    &n,
		    ph->pipe_rd_ovrlp);
    ph->pipe_rd_buf_size = 0;

    if (flag) {
	/* should not happen, but we handle it */
	ph->pipe_rd_buf_size = n;
	if (n == 0) ph->pipe_rd_eof = 1;
	dprintf("PIPE start_reading %u success n=%u\n",
		ph->pipe_handle, n);
    } 
    else {
	err = GetLastError();
	if (err == ERROR_IO_PENDING) {
	    ph->pipe_rd_ovrlp_started = 1;
	    dprintf("PIPE start_reading %u pending\n",
		    ph->pipe_handle);
	}
	else if (err == ERROR_HANDLE_EOF) {
	    ph->pipe_rd_eof = 1;
	    dprintf("PIPE start_reading %u error_handle_eof\n",
		    ph->pipe_handle);
	}
	else {
	    ph->pipe_error_rd = err;
	    dprintf("PIPE start_reading %u error code=%u\n",
		    ph->pipe_handle, err);
	}
    }
}


static void start_writing(struct pipe_helper *ph) {
    int flag;
    DWORD err;

    dprintf("PIPE start_writing %u\n",
	    ph->pipe_handle);

    flag = WriteFile(ph->pipe_handle,
		     ph->pipe_wr_buf,
		     ph->pipe_wr_buf_size,
		     NULL,
		     ph->pipe_wr_ovrlp);
    if (flag) {
	/* should not happen, but we handle it */
	ph->pipe_wr_buf_size = 0;
	dprintf("PIPE start_writing %u success\n",
		ph->pipe_handle);
    } 
    else {
	err = GetLastError();
	if (err == ERROR_IO_PENDING) {
	    ph->pipe_wr_ovrlp_started = 1;
	    dprintf("PIPE start_writing %u pending\n",
		    ph->pipe_handle);
	}
	else {
	    ph->pipe_error_wr = err;
	    dprintf("PIPE start_writing %u error code=%u\n",
		    ph->pipe_handle, err);
	}
    }
}


static void check_for_pending_operations(struct pipe_helper *ph) {
    int flag;
    DWORD n;
    DWORD err;
    dprintf("PIPE check_for_pending %u\n",
	    ph->pipe_handle);

    if (ph->pipe_rd_ovrlp_started) {
	flag = GetOverlappedResult(ph->pipe_handle,
				   ph->pipe_rd_ovrlp,
				   &n,
				   0);
	if (flag) {
	    /* operation is done */
	    dprintf("PIPE check_for_pending %u rd success n=%u\n",
		    ph->pipe_handle, n);
	    ph->pipe_rd_buf_size = n;
	    ph->pipe_rd_ovrlp_started = 0;
	}
	else {
	    err = GetLastError();
	    if (err == ERROR_HANDLE_EOF || err == ERROR_BROKEN_PIPE || 
		err == ERROR_NO_DATA
	       ) {
		dprintf("PIPE check_for_pending %u rd eof code=%u\n",
			ph->pipe_handle, err);
		ph->pipe_rd_buf_size = 0;
		ph->pipe_rd_eof = 1;
		ph->pipe_rd_ovrlp_started = 0;
	    } else if (err != ERROR_IO_INCOMPLETE) {
		ph->pipe_error_rd = err;  /* is reported by next pipe_read */
		ph->pipe_rd_ovrlp_started = 0;
		dprintf("PIPE check_for_pending %u rd error code=%u\n",
			ph->pipe_handle, err);
	    } else {
		dprintf("PIPE check_for_pending %u rd pending\n",
			ph->pipe_handle);
	    }
	}
    };
    if (ph->pipe_wr_ovrlp_started || ph->pipe_cn_ovrlp_started) {
	flag = GetOverlappedResult(ph->pipe_handle,
				   ph->pipe_wr_ovrlp,
				   &n,
				   0);
	if (flag) {
	    /* operation is done */
	    dprintf("PIPE check_for_pending %u wr success n=%u\n",
		    ph->pipe_handle, n);
	    if (ph->pipe_wr_ovrlp_started) {
		/* We assume that always the whole buffer is written! */
		ph->pipe_wr_buf_size = 0;
	    };
	    if (ph->pipe_cn_ovrlp_started) {
		ph->pipe_conn_state = PIPE_CONNECTED;
		if (ph->pipe_mode_rd)
		    start_reading(ph);
		SetEvent(ph->pipe_wr_ev);
	    };
	    ph->pipe_wr_ovrlp_started = 0;
	    ph->pipe_cn_ovrlp_started = 0;
	}
	else {
	    err = GetLastError();
	    if (err != ERROR_IO_INCOMPLETE) {
		ph->pipe_error_wr = err;  /* is reported by next pipe_write */
		if (ph->pipe_cn_ovrlp_started) {
		    ph->pipe_error_rd = err;  /* also report via pipe_read */
		}
		ph->pipe_wr_ovrlp_started = 0;
		ph->pipe_cn_ovrlp_started = 0;
		dprintf("PIPE check_for_pending %u wr error code=%u\n",
			ph->pipe_handle, err);
	    } else {
		dprintf("PIPE check_for_pending %u wr pending\n",
			ph->pipe_handle);
	    }
	}
    };
}
#endif


CAMLprim value netsys_create_local_named_pipe(value name, value mode, value nv)
{
#ifdef _WIN32
    HANDLE h;
    DWORD omode;
    int mode_rd, mode_wr;
    DWORD n;
    struct pipe_helper *ph;
    value r;

    omode = 0;
    mode_rd = 0;
    mode_wr = 0;
    switch (Int_val(mode)) {
    case 0: /* Pipe_in */
	omode = PIPE_ACCESS_INBOUND; mode_rd = 1; break;
    case 1: /* Pipe_out */
	omode = PIPE_ACCESS_OUTBOUND; mode_wr = 1; break;
    case 2: /* Pipe_duplex */
	omode = PIPE_ACCESS_DUPLEX; mode_rd = 1; mode_wr = 1; break;
    };

    n = Int_val(nv);
    if (n <=0 || n > PIPE_UNLIMITED_INSTANCES) n = PIPE_UNLIMITED_INSTANCES;
    
    h = CreateNamedPipe(String_val(name),
			omode | FILE_FLAG_OVERLAPPED,
			PIPE_TYPE_BYTE | PIPE_READMODE_BYTE | PIPE_WAIT,
			n,
			PIPE_HELPER_BUF_SIZE,
			PIPE_HELPER_BUF_SIZE,
			0,
			NULL);
    if ( h == INVALID_HANDLE_VALUE ) {
	win32_maperr(GetLastError());
	uerror("CreateNamedPipe", Nothing);
    }

    /* TODO: set up security vector
       see
       http://msdn.microsoft.com/en-us/library/aa446595(VS.85).aspx
       AllocateAndInitializeSid(SECURITY_NT_AUTHORITY,
                                1, 
			        SECURITY_NETWORK_RID, 0, 0, 0, 0, 0, 0, 0,
			        &psid);
    */

    ph = alloc_pipe_helper(h);
    ph->pipe_is_server = 1;
    ph->pipe_mode_rd = mode_rd;
    ph->pipe_mode_wr = mode_wr;

    r = caml_alloc_custom(&pipe_helper_ops, sizeof(struct pipehelper *), 1, 0);
    *(pipe_helper_ptr_val(r)) = ph;

    dprintf("PIPE create_local_named_pipe %u successful\n",
	    ph->pipe_handle);
    return r;
#else
    invalid_argument("netsys_create_local_named_pipe");
#endif
}


CAMLprim value netsys_pipe_listen(value phv) {
#ifdef _WIN32
    struct pipe_helper *ph;
    int flag;
    DWORD err;

    ph = *(pipe_helper_ptr_val(phv));
    
    dprintf("PIPE listen %u\n",
	    ph->pipe_handle);

    if (ph->pipe_is_open)
	check_for_pending_operations(ph);

    if (!ph->pipe_is_open) {
	errno = EBADF;
	uerror("netsys_pipe_listen", Nothing);
    };

    if (!ph->pipe_is_server) {
	errno = EPERM;
	uerror("netsys_pipe_listen", Nothing);
    };

    if (ph->pipe_conn_state != PIPE_DEAF) {
	errno = EISCONN;
	uerror("netsys_pipe_listen", Nothing);
    };

    if (ph->pipe_cn_ovrlp_started) {
	errno = EALREADY;
	uerror("netsys_pipe_listen", Nothing);
    };

    dprintf("PIPE listen %u connecting\n",
	    ph->pipe_handle);

    flag = ConnectNamedPipe(ph->pipe_handle,
			    ph->pipe_wr_ovrlp);
    if (flag) {
	/* immediate success */
	ph->pipe_conn_state = PIPE_CONNECTED;
	SetEvent(ph->pipe_wr_ev);
	dprintf("PIPE listen %u connected 1\n",
		ph->pipe_handle);
	
    } else {
	err = GetLastError();
	switch (err) {
	case ERROR_PIPE_CONNECTED:
	    /* also immediate success */
	    ph->pipe_conn_state = PIPE_CONNECTED; 
	    SetEvent(ph->pipe_wr_ev);
	    dprintf("PIPE listen %u connected 2\n",
		    ph->pipe_handle);
	    break;
	case ERROR_IO_PENDING:
	    /* we connect in the background */
	    ph->pipe_cn_ovrlp_started = 1; 
	    ph->pipe_conn_state = PIPE_LISTENING;
	    dprintf("PIPE listen %u pending\n",
		    ph->pipe_handle);
	    break;
	default:
	    dprintf("PIPE listen %u error err=%u\n",
		    ph->pipe_handle, err);
	    win32_maperr(err);
	    uerror("ConnectNamedPipe", Nothing);
	}
    };

    return Val_unit;
#else
    invalid_argument("netsys_pipe_listen");
#endif
}


CAMLprim value netsys_pipe_deafen(value phv) {
#ifdef _WIN32
    struct pipe_helper *ph;
    int flag;

    ph = *(pipe_helper_ptr_val(phv));
    
    dprintf("PIPE deafen %u\n",
	    ph->pipe_handle);

    if (ph->pipe_is_open)
	check_for_pending_operations(ph);

    if (!ph->pipe_is_open) {
	errno = EBADF;
	uerror("netsys_pipe_deafen", Nothing);
    };

    if (!ph->pipe_is_server) {
	errno = EPERM;
	uerror("netsys_pipe_deafen", Nothing);
    };

    if (ph->pipe_conn_state == PIPE_DEAF) {
	errno = ENOTCONN;
	uerror("netsys_pipe_deafen", Nothing);
    };

    dprintf("PIPE deafen %u disconnecting\n",
	    ph->pipe_handle);

    flag = DisconnectNamedPipe(ph->pipe_handle);
    if (!flag) {
	win32_maperr(GetLastError());
	uerror("DisconnectNamedPipe", Nothing);
    }

    /* Check whether the overlapped ops are done: */
    check_for_pending_operations(ph);
    if (ph->pipe_cn_ovrlp_started) {
	failwith("netsys_pipe_unlisten: cannot stop pending ConnectNamedPipe");
    };
    if (ph->pipe_rd_ovrlp_started) {
	failwith("netsys_pipe_unlisten: cannot stop pending ReadFile");
    };
    if (ph->pipe_wr_ovrlp_started) {
	failwith("netsys_pipe_unlisten: cannot stop pending WriteFile");
    };

    dprintf("PIPE deafen %u successful\n",
	    ph->pipe_handle);

    ph->pipe_conn_state = PIPE_DEAF;
    ph->pipe_error_rd = 0;
    ph->pipe_error_wr = 0;
    ph->pipe_rd_buf_size = 0;
    ph->pipe_rd_eof = 0;
    ph->pipe_wr_buf_size = 0;

    ResetEvent(ph->pipe_rd_ev);
    ResetEvent(ph->pipe_wr_ev);

    return Val_unit;

#else
    invalid_argument("netsys_pipe_unlisten");
#endif
}


CAMLprim value netsys_pipe_connect(value name, value mode) {
#ifdef _WIN32
    HANDLE h;
    DWORD omode;
    int mode_rd, mode_wr;
    DWORD err;
    struct pipe_helper *ph;
    value r;

    omode = 0;
    mode_rd = 0;
    mode_wr = 0;
    switch (Int_val(mode)) {
    case 0: /* Pipe_in */
	omode = GENERIC_READ; mode_rd = 1; break;
    case 1: /* Pipe_out */
	omode = GENERIC_WRITE; mode_wr = 1; break;
    case 2: /* Pipe_duplex */
	omode = GENERIC_READ | GENERIC_WRITE; mode_rd = 1; mode_wr = 1; break;
    };

    h = CreateFile(String_val(name),
		   omode,
		   0,
		   NULL,
		   OPEN_EXISTING,
		   FILE_FLAG_OVERLAPPED,
		   NULL);
    if ( h == INVALID_HANDLE_VALUE ) {
	err = GetLastError();
	if ( err == ERROR_PIPE_BUSY )
	    errno = EAGAIN;
	else
	    win32_maperr(err);
	uerror("CreateFile", Nothing);
    };

    ph = alloc_pipe_helper(h);
    ph->pipe_conn_state = PIPE_CONNECTED;
    ph->pipe_mode_rd = mode_rd;
    ph->pipe_mode_wr = mode_wr;
    SetEvent(ph->pipe_wr_ev);

    r = caml_alloc_custom(&pipe_helper_ops, sizeof(struct pipehelper *), 1, 0);
    *(pipe_helper_ptr_val(r)) = ph;

    dprintf("PIPE connect %u successful\n",
	    ph->pipe_handle);

    if (mode_rd)
	start_reading(ph);

    return r;

#else
    invalid_argument("netsys_pipe_connect");
#endif
}


CAMLprim value netsys_pipe_read(value phv, value s, value pos, value len) {
#ifdef _WIN32
    struct pipe_helper *ph;
    int flag;
    int l;

    ph = *(pipe_helper_ptr_val(phv));
    l = Int_val(len);
    
    dprintf("PIPE read %u len=%d\n",
	    ph->pipe_handle, l);

    if (ph->pipe_is_open)
	check_for_pending_operations(ph);

    if (ph->pipe_error_rd != 0) {
	win32_maperr(ph->pipe_error_rd);
	uerror("netsys_pipe_read", Nothing);
    };

    if (l == 0) 
	return Val_int(0);

    if (!ph->pipe_is_open || !ph->pipe_mode_rd) {
	errno = EBADF;
	uerror("netsys_pipe_read", Nothing);
    };

    if (ph->pipe_conn_state != PIPE_CONNECTED) {
	errno = ENOTCONN;
	uerror("netsys_pipe_read", Nothing);
    };

    if (ph->pipe_rd_ovrlp_started) {
	dprintf("PIPE read %u eagain\n",
		ph->pipe_handle);

	errno = EAGAIN;
	uerror("netsys_pipe_read", Nothing);
    };

    if (ph->pipe_rd_buf_size < l) 
	l = ph->pipe_rd_buf_size;

    CopyMemory(String_val(s) + Int_val(pos), ph->pipe_rd_buf, l);
    MoveMemory(ph->pipe_rd_buf, ph->pipe_rd_buf+l, ph->pipe_rd_buf_size-l);
    ph->pipe_rd_buf_size = ph->pipe_rd_buf_size - l;

    if (ph->pipe_rd_buf_size == 0 && !ph->pipe_rd_eof)
	start_reading(ph);

    dprintf("PIPE read %u returning %d\n",
	    ph->pipe_handle, l);

    return Val_int(l);
#else
    invalid_argument("netsys_pipe_read");
#endif
}


CAMLprim value netsys_pipe_write(value phv, value s, value pos, value len) {
#ifdef _WIN32
    struct pipe_helper *ph;
    int flag;
    int l;

    ph = *(pipe_helper_ptr_val(phv));
    l = Int_val(len);
    
    dprintf("PIPE write %u %d\n",
	    ph->pipe_handle, l);

    if (ph->pipe_is_open)
	check_for_pending_operations(ph);

    if (ph->pipe_error_wr != 0) {
	if (ph->pipe_error_wr == ERROR_PIPE_NOT_CONNECTED ||
	    ph->pipe_error_wr == ERROR_NO_DATA
	    ) {
	    errno = EPIPE;
	    dprintf("PIPE write %u epipe\n",
		    ph->pipe_handle);
	}
	else
	    win32_maperr(ph->pipe_error_wr);
	uerror("netsys_pipe_write", Nothing);
    };

    if (l == 0) 
	return Val_int(0);

    if (!ph->pipe_is_open || !ph->pipe_mode_wr) {
	errno = EBADF;
	uerror("netsys_pipe_write", Nothing);
    };

    if (ph->pipe_conn_state != PIPE_CONNECTED) {
	errno = ENOTCONN;
	uerror("netsys_pipe_write", Nothing);
    };

    if (ph->pipe_wr_ovrlp_started) {
	dprintf("PIPE write %u eagain\n",
	       ph->pipe_handle);
	errno = EAGAIN;
	uerror("netsys_pipe_write", Nothing);
    };

    if (l > PIPE_HELPER_BUF_SIZE) 
	l = PIPE_HELPER_BUF_SIZE;

    CopyMemory(ph->pipe_wr_buf, String_val(s) + Int_val(pos), l);
    ph->pipe_wr_buf_size = l;

    if (l > 0)
	start_writing(ph);

    dprintf("PIPE write %u returning %d\n",
	    ph->pipe_handle, l);

    return Val_int(l);
#else
    invalid_argument("netsys_pipe_write");
#endif
}


CAMLprim value netsys_pipe_shutdown(value phv) {
#ifdef _WIN32
    struct pipe_helper *ph;
    int flag;

    ph = *(pipe_helper_ptr_val(phv));

    dprintf("PIPE shutdown %u is_open=%d\n",
	    ph->pipe_handle, ph->pipe_is_open);

    if (ph->pipe_is_open) {
	if (ph->pipe_conn_state == PIPE_DEAF) {
	    errno = ENOTCONN;
	    uerror("netsys_pipe_shutdown", Nothing);
	};

	flag = CloseHandle(ph->pipe_handle);
	if (!flag) {
	    win32_maperr(GetLastError());
	    uerror("CloseHandle", Nothing);
	};
	ph->pipe_is_open = 0;
	ph->pipe_conn_state = PIPE_DOWN;
	if (ph->pipe_signal != NULL)
	    SetEvent(ph->pipe_signal);
	ResetEvent(ph->pipe_rd_ev);
	ResetEvent(ph->pipe_wr_ev);
    }

    return Val_unit;
#else
    invalid_argument("netsys_pipe_shutdown");
#endif
}


CAMLprim value netsys_pipe_free(value phv) {
#ifdef _WIN32
    struct pipe_helper *ph;

    ph = *(pipe_helper_ptr_val(phv));
    free_pipe_helper(ph);
    *(pipe_helper_ptr_val(phv)) = NULL;

    return Val_unit;
#else
    invalid_argument("netsys_pipe_free");
#endif
}


CAMLprim value netsys_pipe_conn_state(value phv) {
#ifdef _WIN32
    struct pipe_helper *ph;

    ph = *(pipe_helper_ptr_val(phv));
    return Val_int(ph->pipe_conn_state);
#else
    invalid_argument("netsys_pipe_conn_state");
#endif
}


CAMLprim value netsys_pipe_rd_event(value phv) {
#ifdef _WIN32
    value r;
    struct pipe_helper *ph;
    struct event e0;

    ph = *(pipe_helper_ptr_val(phv));

    if (!ph->pipe_is_open) {
	errno = EBADF;
	uerror("netsys_pipe_rd_event", Nothing);
    };

    e0.ev = ph->pipe_rd_ev;
    e0.mask = 0;

    r = caml_alloc_custom(&event_ops, sizeof(struct event), 1, 0);
    *(event_val(r)) = e0;

    return r;
#else
    invalid_argument("netsys_pipe_rd_event");
#endif
}


CAMLprim value netsys_pipe_wr_event(value phv) {
#ifdef _WIN32
    struct pipe_helper *ph;
    struct event e0;
    value r;

    ph = *(pipe_helper_ptr_val(phv));

    if (!ph->pipe_is_open) {
	errno = EBADF;
	uerror("netsys_pipe_wr_event", Nothing);
    };

    e0.ev = ph->pipe_wr_ev;
    e0.mask = 0;

    r = caml_alloc_custom(&event_ops, sizeof(struct event), 1, 0);
    *(event_val(r)) = e0;

    return r;
#else
    invalid_argument("netsys_pipe_wr_event");
#endif
}


CAMLprim value netsys_pipe_descr(value phv) {
#ifdef _WIN32
    struct pipe_helper *ph;

    ph = *(pipe_helper_ptr_val(phv));

    return netsysw32_win_alloc_handle(ph->pipe_descr);
#else
    invalid_argument("netsys_pipe_descr");
#endif
}

CAMLprim value netsys_pipe_signal(value phv, value ev) {
#ifdef _WIN32
    struct pipe_helper *ph;
    struct event e;

    ph = *(pipe_helper_ptr_val(phv));
    e = *(event_val(ev));
    ph->pipe_signal = e.ev;

    return Val_unit;
#else
    invalid_argument("netsys_pipe_signal");
#endif
}





