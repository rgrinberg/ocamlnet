/* Speed of decoding XDR ints */

/* Opteron 1354 with 8 GB RAM, 64 bit mode:

   Time reading array: 0.012689   (around 1.2ns per loop cycle)
   Time w/ endianess fixing: 0.016352 (around 1.6ns per loop cycle)
*/

#include <unistd.h>
#include <time.h>
#include <sys/time.h>
#include <stdio.h>
#include <stdlib.h>
#include <arpa/inet.h>

int main () {
    char *c;
    int k,p;
    struct timeval t0, t1;
    double d0, d1;
    int i;
    int sum;

    c = (char *) malloc(40000000);
    for (k=0; k <= 9999999; k++) {
	p = 4*k;
	c[p] = 0x10;
	c[p+1] = 0x11;
	c[p+2] = 0x12;
	c[p+3] = 0x13;
    }

    gettimeofday(&t0, NULL);
    k = 0;
    sum = 0;
    while (k < 40000000) {
	i = * ((int *) (c+k));
	sum += i;
	k += 4;
    }
    gettimeofday(&t1, NULL);


    d0 = t0.tv_sec + 0.000001 * t0.tv_usec;
    d1 = t1.tv_sec + 0.000001 * t1.tv_usec;

    printf("Sum: %d\n", sum);
    printf("Time reading array: %f\n", d1-d0);

    gettimeofday(&t0, NULL);
    k = 0;
    sum = 0;
    while (k < 40000000) {
	i = (int) ntohl(* ((unsigned int *) (c+k)));
	sum += i;
	k += 4;
    }
    gettimeofday(&t1, NULL);

    d0 = t0.tv_sec + 0.000001 * t0.tv_usec;
    d1 = t1.tv_sec + 0.000001 * t1.tv_usec;

    printf("Sum: %d\n", sum);
    printf("Time endianess fixing: %f\n", d1-d0);
  

    return 0;
}
