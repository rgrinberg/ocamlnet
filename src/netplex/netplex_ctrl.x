/* $Id$ -*- c -*- */

typedef string longstring<>;

typedef longstring *internal_port;
/* The path of a Unix domain socket if the service is found and enabled */

struct message {
    longstring msg_name;
    longstring msg_arguments<>;
};


struct socket_id {
    longstring sock_protocol;   /* name of the protocol */
    int        sock_index;      /* array index */
};


typedef socket_id socket_id_list<>;


enum event_type {
    EVENT_ACCEPT = 0,
    EVENT_RECEIVED_MESSAGE = 1,
    EVENT_RECEIVED_ADMIN_MESSAGE = 2,
    EVENT_SHUTDOWN = 3
};


union event switch(event_type discr) {
 case EVENT_ACCEPT:
     socket_id_list enabled_sockets;
     /* Sets that these sockets try to accept new connections. */
 case EVENT_RECEIVED_MESSAGE:
     message msg;
 case EVENT_RECEIVED_ADMIN_MESSAGE:
     message msg;
 case EVENT_SHUTDOWN:
     void;
};


enum level {
    LOG_EMERG = 0,
    LOG_ALERT = 1,
    LOG_CRIT = 2,
    LOG_ERR = 3,
    LOG_WARNING = 4,
    LOG_NOTICE = 5,
    LOG_INFO = 6,
    LOG_DEBUG = 7
};



program Control {
    /* Internal API between controller and container */

    version V1 {

	void ping(void) = 0;

	internal_port lookup(longstring,        /* service name */
			     longstring         /* protocol */
			     ) = 1;

	void send_message(longstring,           /* service name */
			  message               /* message */
			  ) = 2;
	/* Service names may contain "*" as wildcard. For example,
         * send_message("*", msg) broadcasts to all processors.
         */


	event wait(int               /* Number of active connections */
		   ) = 3;
	/* Waits for the next controller event */

	void accepted(socket_id) = 4;
	/* Tells the controller that a connection on this socket has just
         * been accepted. 
         *
         * This is a special procedure: The controller does not send a
         * response for performance reasons.
         */

	void log(level,                /* log level */
		 longstring            /* log message */
		 ) = 5;
        /* This is a special procedure: The controller does not send a
         * response for performance reasons.
         */

    } = 1;

} = 1;


enum result_code {
    CODE_OK = 0,
    CODE_ERROR = 1
};


union unit_result switch (result_code discr) {
 case CODE_OK:
     void;
 case CODE_ERROR:
     longstring message;
};


enum socket_domain {
    PF_UNIX = 0,
    PF_INET = 1,
    PF_INET6 = 2
};


union port switch (socket_domain discr) {
 case PF_UNIX:
     longstring path;
 case PF_INET:
     struct {
	 longstring inet_addr;
	 int inet_port;
     } inet;
 case PF_INET6:
     struct {
	 longstring inet6_addr;
	 int inet6_port;
     } inet6;
};


typedef port port_list<>;


struct prot {
    longstring prot_name;
    port_list  prot_ports;
};


typedef prot prot_list<>;


enum srv_state {
    STATE_ENABLED = 0,
    STATE_DISABLED = 1,
    STATE_RESTARTING = 2,
    STATE_DOWN = 3
};


struct service {
    longstring srv_name;
    prot_list  srv_protocols;
    int        srv_nr_containers;
    srv_state  srv_state;
};


typedef service service_list<>;


program Admin {
    /* User API, accessible from the outside */

    version V1 {

	void ping(void) = 0;

	service_list list(void) = 1;
	/* list of services: name, protocols, ports, state */

	unit_result enable(longstring          /* service name */
			   ) = 2;

	unit_result disable(longstring         /* service name */
			    ) = 3;

	unit_result restart(longstring         /* service name */
			    ) = 4;
	unit_result restart_all(void) = 5;

	unit_result shutdown(void) = 6;

	unit_result reopen_logfiles(void) = 7;
	/* reopen logfiles */

	void send_admin_message(longstring,           /* service name */
				message               /* message */
				) = 8;
	/* Service names may contain "*" as wildcard. For example,
         * send_admin_message("*", msg) broadcasts to all processors.
         */


    } = 1;

} = 2;
