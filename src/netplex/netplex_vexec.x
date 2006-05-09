/* $Id$ -*- c -*- */

/* VEXEC protocol */

program VEXEC_initiator {
    version V1 {

	void ping(void) = 0;

	longstring initiate(void) = 1;
	/* Initiates a session and returns the path of the Unix domain socket
         * where the session provider can be connected.
         */

    } = 1;

} = 1;


program VEXEC_session {
    version V1 {
	void ping(void) = 0;

	int invoke(string_array,    /* cmd */
		   string_array     /* env */
		   ) = 1;
	/* Invoke the command with cmd and env. A session identifier is
	 * returned. It is expected that the client connects four more
         * times:
         * - for each channel (stdin, stdout, stderr): channel
         * - to wait until command completion: wait
         */

	void channel(int,            /* session identifier */
		     channel_type    /* which channel */
		     ) = 2;
	/* Connects the channel. After the client gets the response,
         * the stream connection takes over the function of the channel.
         */

	int wait(int                 /* session identifier */
		 ) = 3;
	/* Waits until the command is finished and returns the exit code */

    } = 1;

} = 2;
