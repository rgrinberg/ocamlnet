/* $Id$ -*- c -*- */

program P {
    version V1 {
	void ping(void) = 0;

	int hard_work(void) = 1;
	/* Sleeps one second, the returns "1" */

	void fail(void) = 2;
	/* Simply fails */

	void exit(void) = 3;
	/* Exit with code 3 */

    } = 1;
} = 1;
