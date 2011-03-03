/* $Id$
 * ----------------------------------------------------------------------
 *
 */

typedef string       longstring<>;
typedef longstring  *longstring_opt;

program PROTO {
    version V1 {
	void ping(void) = 0;
	/* Just return. */

	longstring revert(longstring) = 1;
	/* Return the reverted string */

	longstring batch_in(bool, bool, longstring) = 2;
	/* Batching for incoming messages:
	 * The client sends one initiating message, several follow-up
	 * messages, and after the last message the server responds.
	 * batch_in(true, _, s): This message is the first message of the
	 *   sequence of messages. The string s is the first string of this
	 *   sequence.
	 * batch_in(_, false, s): The server will not reply to this message
	 *   but just record the string s.
	 * batch_in(_, true, s): This message is the last message of the
	 *   sequence. The server will reply to the whole sequence by returning
	 *   the concatenation of all strings.
	 */

	longstring_opt batch_out(longstring) = 3;
	/* Batching for outgoing messages:
	 * The client sends one initiating message, and the server responds
	 * multiple times.
	 * batch_out s: The call is sent exactly once from the client to
	 *   the server.
	 * The server responds with (Some s') several times. The last message
	 * from the server is None. The concatenation of all responses s'
	 * is equal to s.
	 */

	void retransmit1(void) = 4;
	void retransmit2(void) = 5;
	/* retransmit1() resets the retransmission status. This means that the
	 * next call of retransmit2 is not replied, but the second next call.
	 */

	void install_dropping_filter(void) = 6;
	/* Installs a filter that drops the next call. The filter will be
	 * removed after the call.
	 */

	void install_rejecting_filter(void) = 7;
	/* Installs a filter that rejects the next call. The filter will be
	 * removed after the call.
	 */

	void install_denying_filter(void) = 8;
	/* Installs a filter that denies the next call. The filter will be
	 * removed after the call.
	 */

	void install_dropping_filter_with_limit(void) = 9;
	/* Installs a filter that drops the next call if it is longer than
	 * 80 bytes. The filter will be removed after the call.
	 */

	void auth_sys(void) = 10;
	/* Requires user 50, group 51, groups [52,53]. */

	void enable_auth_local(void) = 11;
	/* Enables AUTH_LOCAL authentication */

	longstring auth_local(void) = 12;
	/* Returns the user as identified by AUTH_LOCAL */

	longstring auth_dh(void) = 13;
	/* Returns the user as identified by AUTH_DH */

	longstring auth_scram(void) = 14;
	/* Returns the user as identified by RPCSEC_GSS with SCRAM */

    } = 1;
} = 0x33333333;
