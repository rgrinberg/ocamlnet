/* $Id$ -*-c -*- */

/* A very simple program only providing one [operation]. The examples
   in this dir use this definition.
*/

typedef string longstring<>;

program P {
    version V {
	void null(void) = 0;
	longstring operation(longstring) = 1;
    } = 1;
} = 1;
