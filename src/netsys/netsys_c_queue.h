/* $Id$ */

/* Queue for netsys_mem.c */

#ifndef NETSYS_C_QUEUE
#define NETSYS_C_QUEUE

struct queue {
    void          **table;
    unsigned long   table_size;
    unsigned long   table_start;
    unsigned long   table_end;
};


extern int netsys_queue_init(struct queue *q, unsigned long n);
/* Initializes the queue q for n cells.

   Return 0 on success, or (-1) on system error (errno), or (-2) on
   library error. On success, the structure [t] is initialized.
*/

extern int netsys_queue_add(struct queue *q, void *elem);
/* Adds elem to the end of the queue. The queue is resized if required.
   elem must be non-NULL.

   Return 0 on success, or (-1) on system error (errno), or (-2) on
   library error.
 */

extern int netsys_queue_take(struct queue *q, void **elem);
/* Takes an element from the beginning of the queue, and puts it into
   *elem. If the queue is empty, NULL is put into *elem.

   Return 0 on success, or (-1) on system error (errno), or (-2) on
   library error, or (-3) when the queue is empty.
 */

extern void netsys_queue_free(struct queue *q);
/* Frees the memory allocated by q */

#endif

