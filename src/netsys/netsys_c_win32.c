/* $Id$ */

/* Note: need -lws2_32 to link this file, e.g. */
/* ocamlmktop -o otop -I . -cclib -lws2_32 unix.cma netsys.cma */

#include "netsys_c.h"
#include <stdio.h>

#ifdef _WIN32

static struct custom_operations wsaevent_ops = {
    "",
    custom_finalize_default,
    custom_compare_default,
    custom_hash_default,
    custom_serialize_default,
    custom_deserialize_default
};


struct event {
    WSAEVENT wsaev;
    int      mask;
};



#define event_val(v) ((struct event *) (Data_custom_val(v)))


value netsys_wsa_create_event(value dummy) {
    WSAEVENT e;
    value r;
    struct event e0;

    e = WSACreateEvent();
    if (e == WSA_INVALID_EVENT) {
	win32_maperr(WSAGetLastError());
	uerror("WSACreateEvent", Nothing);
    }
    e0.wsaev = e;
    e0.mask = 0;

    r = caml_alloc_custom(&wsaevent_ops, sizeof(struct event), 1, 0);
    *(event_val(r)) = e0;

    return r;
}

value netsys_wsa_close_event(value ev) {
    struct event e;

    e = *(event_val(ev));
    if (!WSACloseEvent(e.wsaev)) {
	win32_maperr(WSAGetLastError());
	uerror("WSACloseEvent", Nothing);
    }

    return Val_unit;
}

value netsys_wsa_event_select(value ev, value fdv, value evmaskv) {
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

    if (WSAEventSelect(s, e.wsaev, m_win32) != 0) {
	win32_maperr(WSAGetLastError());
	uerror("WSAEventSelect", Nothing);
    }

    return Val_unit;
}

value netsys_wsa_maximum_wait_events(value dummy) {
    return Val_int(WSA_MAXIMUM_WAIT_EVENTS);
}

value netsys_wsa_wait_for_multiple_events(value fdarray, value tmov) {
    WSAEVENT earray[WSA_MAXIMUM_WAIT_EVENTS];
    struct event e;
    DWORD tmo;
    int k,n;
    DWORD r;
    value rv;

    tmo = Long_val(tmov);
    n = Wosize_val(fdarray);

    for (k=0; k < n; k++) {
	e = *(event_val(Field(fdarray,k)));
	earray[k] = e.wsaev;
    }

    if (n == 0) {
	r = SleepEx(tmo, 1);
	
	if (r == WAIT_IO_COMPLETION) {
	    win32_maperr(EINTR);
	    uerror("SleepEx", Nothing);
	}

	return Val_int(0);    /* None */
    }
    else {
	r = WSAWaitForMultipleEvents(n, earray, 0, tmo, 1);
    
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
}

value netsys_wsa_enum_network_events(value fdv, value ev) {
    struct event e;
    SOCKET s;
    WSANETWORKEVENTS ne;
    int r;

    e = *(event_val(ev));
    s = Socket_val(fdv);

    if (WSAEnumNetworkEvents(s, e.wsaev, &ne) != 0) {
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
}

/* CHECK:
   - maybe we have to make the FD_CLOSE event last forever (it is not
     level-triggered)
*/


#endif
