/* $Id$ */

#include "netsys_c.h"
#include "netsys_c_htab.h"
#include "netsys_c_queue.h"

#ifdef HAVE_MMAP
#include <sys/types.h>
#include <sys/mman.h>
#if !defined(MAP_ANON) && defined(MAP_ANONYMOUS)
#define MAP_ANON MAP_ANONYMOUS
#endif
#endif

/**********************************************************************/
/* Bigarray helpers                                                   */
/**********************************************************************/

CAMLprim value netsys_blit_memory_to_string(value memv,
					    value memoffv,
					    value sv,
					    value soffv,
					    value lenv)
{
    struct caml_bigarray *mem = Bigarray_val(memv);
    char * s = String_val(sv);
    long memoff = Long_val(memoffv);
    long soff = Long_val(soffv);
    long len = Long_val(lenv);

    memmove(s + soff, ((char*) mem->data) + memoff, len);

    return Val_unit;
}


CAMLprim value netsys_blit_string_to_memory(value sv,
					    value soffv,
					    value memv,
					    value memoffv,
					    value lenv)
{
    struct caml_bigarray *mem = Bigarray_val(memv);
    char * s = String_val(sv);
    long memoff = Long_val(memoffv);
    long soff = Long_val(soffv);
    long len = Long_val(lenv);

    memmove(((char*) mem->data) + memoff, s + soff, len);

    return Val_unit;
}


CAMLprim value netsys_memory_address(value memv)
{
    struct caml_bigarray *mem = Bigarray_val(memv);
    return caml_copy_nativeint((intnat) mem->data);
}


CAMLprim value netsys_getpagesize(value dummy)
{
#ifdef HAVE_SYSCONF
    return Val_long(sysconf(_SC_PAGESIZE));
#else
    invalid_argument("Netsys_mem.getpagesize not available");
#endif
}


CAMLprim value netsys_alloc_memory_pages(value addrv, value pv)
{
#if defined(HAVE_MMAP) && defined(HAVE_SYSCONF) && defined(MAP_ANON)
    void *start;
    size_t length;
    long pgsize;
    void *data;
    value r;

    start = (void *) Nativeint_val(addrv);
    if (start == 0) start=NULL;    /* for formal reasons */

    length = Int_val(pv);
    pgsize = sysconf(_SC_PAGESIZE);
    length = ((length - 1) / pgsize + 1) * pgsize;  /* fixup */

    data = mmap(start, length, PROT_READ|PROT_WRITE, 
		MAP_PRIVATE | MAP_ANON, (-1), 0);
    if (data == (void *) -1) uerror("mmap", Nothing);

    r = alloc_bigarray_dims(BIGARRAY_C_LAYOUT | BIGARRAY_UINT8 | 
			    BIGARRAY_MAPPED_FILE,
			    1, data, length);

    return r;
#else
    invalid_argument("Netsys_mem.alloc_memory_pages not available");
#endif
}


CAMLprim value netsys_alloc_aligned_memory(value alignv, value pv)
{
#if defined(HAVE_POSIX_MEMALIGN)
    size_t align = Long_val(alignv);
    size_t size = Long_val(pv);
    void * addr = NULL;
    int e;
    value r;

    e = posix_memalign(&addr, align, size);
    if (e != 0) unix_error(e, "posix_memalign", Nothing);

    r = alloc_bigarray_dims(BIGARRAY_C_LAYOUT | BIGARRAY_UINT8 | 
			    BIGARRAY_MANAGED,
			    1, addr, size);
    return r;
#else
    invalid_argument("Netsys_mem.alloc_aligned_memory not available");
#endif
}


CAMLprim value netsys_map_file(value fdv,
			       value posv,
			       value addrv,
			       value sharedv,
			       value sizev)
{
#if defined(HAVE_MMAP) && defined(HAVE_SYSCONF) && !defined(_WIN32)
    int fd, shared;
    off64_t pos, savepos, eofpos;
    void *addr, *eff_addr;
    long size;
    uintnat basize;
    char c;
    uintnat pagesize, delta;

    fd = Int_val(fdv);
    pos = Int64_val(posv);
    addr = (void *) Nativeint_val(addrv);
    if (addr == 0) addr = NULL;
    shared = Bool_val(sharedv) ? MAP_SHARED : MAP_PRIVATE;
    size = Long_val(sizev);

    pagesize = sysconf(_SC_PAGESIZE);

    savepos = lseek64(fd, 0, SEEK_CUR);
    if (savepos == -1) uerror("lseek64", Nothing);
    eofpos = lseek64(fd, 0, SEEK_END);
    if (eofpos == -1) uerror("lseek64", Nothing);
    
    if (size == -1) {
	if (eofpos < pos) 
	    failwith("Netsys_mem: cannot mmap - file position exceeds file size");
	basize = (uintnat) (eofpos - pos);
    }
    else {
	if (size < 0)
	    invalid_argument("netsys_map_file");
	if (eofpos < pos + size) {
	    if (lseek64(fd, pos + size - 1, SEEK_SET) == -1)
		uerror("lseek64", Nothing);
	    c = 0;
	    if (write(fd, &c, 1) != 1) uerror("write", Nothing);
	}
	basize = size;
    }
    lseek64(fd, savepos, SEEK_SET);

    delta = (uintnat) (pos % pagesize);
    eff_addr = mmap64(addr, basize, PROT_READ | PROT_WRITE,
		      shared, fd, pos - delta);
    if (eff_addr == (void*) MAP_FAILED) uerror("mmap64", Nothing);
    eff_addr = (void *) ((uintnat) eff_addr + delta);

    return alloc_bigarray_dims(BIGARRAY_UINT8 | BIGARRAY_C_LAYOUT | 
			       BIGARRAY_MAPPED_FILE, 1, eff_addr, basize);
#else
    invalid_argument("Netsys_mem.memory_map_file not available");
#endif
}


/* from mmap_unix.c: */
static void ba_unmap_file(void * addr, uintnat len)
{
#if defined(HAVE_MMAP) && defined(HAVE_SYSCONF)
  uintnat page = sysconf(_SC_PAGESIZE);
  uintnat delta = (uintnat) addr % page;
  munmap((void *)((uintnat)addr - delta), len + delta);
#endif
}



CAMLprim value netsys_memory_unmap_file(value memv) 
{
    struct caml_bigarray *b = Bigarray_val(memv);
    if ((b->flags & BIGARRAY_MANAGED_MASK) == BIGARRAY_MAPPED_FILE) {
	if (b->proxy == NULL) {
	    ba_unmap_file(b->data, b->dim[0]);
	    b->data = NULL;
	    b->flags = 
		(b->flags & ~BIGARRAY_MANAGED_MASK) | BIGARRAY_EXTERNAL;
	}
	else if (b->proxy->refcount == 1) {
	    ba_unmap_file(b->proxy->data, b->dim[0]);
	    b->proxy->data = NULL;
	    b->data = NULL;
	    b->flags = 
		(b->flags & ~BIGARRAY_MANAGED_MASK) | BIGARRAY_EXTERNAL;
	}
    }
    return Val_unit;
}

/**********************************************************************/
/* I/O with Bigarrays                                                 */
/**********************************************************************/

CAMLprim value netsys_mem_read(value fdv, value memv, value offv, value lenv)
{
    intnat numbytes;
    intnat ret;
    char *data;
#ifdef _WIN32
    DWORD n;
    DWORD err = 0;
#endif

    numbytes = Long_val(lenv);
    data = ((char *) (Bigarray_val(memv)->data)) + Long_val(offv);
#ifdef _WIN32
    if (Descr_kind_val(fdv) == KIND_SOCKET) {
	SOCKET h = Socket_val(fdv);
	enter_blocking_section();
	ret = recv(h, data, numbytes, 0);
	if (ret == SOCKET_ERROR) err = WSAGetLastError();
	leave_blocking_section();
	ret = n;
    } else {
	HANDLE h = Handle_val(fdv);
	enter_blocking_section();
	if (! ReadFile(h, data, numbytes, &n, NULL)) err = GetLastError();
	leave_blocking_section();
	ret = n;
    }
    if (err) {
	win32_maperr(err);
	ret = -1;
    }
#else
    enter_blocking_section();
    ret = read(Int_val(fdv), data, (int) numbytes);
    leave_blocking_section();   /* keeps errno intact */
#endif
    if (ret == -1) uerror("mem_read", Nothing);
    return Val_long(ret);
}


CAMLprim value netsys_mem_write(value fdv, value memv, value offv, value lenv)
{
    intnat numbytes;
    intnat ret;
    char *data;
#ifdef _WIN32
    DWORD n;
    DWORD err = 0;
#endif

    numbytes = Long_val(lenv);
    data = ((char *) (Bigarray_val(memv)->data)) + Long_val(offv);
#ifdef _WIN32
    if (Descr_kind_val(fdv) == KIND_SOCKET) {
	SOCKET h = Socket_val(fdv);
	enter_blocking_section();
	ret = send(h, data, numbytes, 0);
	if (ret == SOCKET_ERROR) err = WSAGetLastError();
	leave_blocking_section();
	ret = n;
    } else {
	HANDLE h = Handle_val(fdv);
	enter_blocking_section();
	if (! WriteFile(h, data, numbytes, &n, NULL)) err = GetLastError();
	leave_blocking_section();
	ret = n;
    }
    if (err) {
	win32_maperr(err);
	ret = -1;
    }
#else
    enter_blocking_section();
    ret = write(Int_val(fdv), data, (int) numbytes);
    leave_blocking_section();
#endif
    if (ret == -1) uerror("mem_write", Nothing);
    return Val_long(ret);
}


static int msg_flag_table[] = {
  MSG_OOB, MSG_DONTROUTE, MSG_PEEK
};


CAMLprim value netsys_mem_recv(value fdv, value memv, value offv, value lenv,
			       value flagsv)
{
    intnat numbytes;
    intnat ret;
    char *data;
    int flags;
#ifdef _WIN32
    DWORD err = 0;
    SOCKET s;
#else
    int s;
#endif

    numbytes = Long_val(lenv);
    data = ((char *) (Bigarray_val(memv)->data)) + Long_val(offv);
    flags = convert_flag_list(flagsv, msg_flag_table);

#ifdef _WIN32
    s = Socket_val(fdv);
#else
    s = Int_val(fdv);
#endif

    enter_blocking_section();
    ret = recv(s, data, (int) numbytes, flags);

#ifdef _WIN32
    if (ret == -1) err = WSAGetLastError();
    leave_blocking_section();
    if (ret == -1) win32_maperr(err);
#else
    leave_blocking_section();
#endif

    if (ret == -1) uerror("mem_recv", Nothing);
    return Val_long(ret);
}


CAMLprim value netsys_mem_send(value fdv, value memv, value offv, value lenv,
			       value flagsv)
{
    intnat numbytes;
    intnat ret;
    char *data;
    int flags;
#ifdef _WIN32
    DWORD err = 0;
    SOCKET s;
#else
    int s;
#endif

    numbytes = Long_val(lenv);
    data = ((char *) (Bigarray_val(memv)->data)) + Long_val(offv);
    flags = convert_flag_list(flagsv, msg_flag_table);

#ifdef _WIN32
    s = Socket_val(fdv);
#else
    s = Int_val(fdv);
#endif

    enter_blocking_section();
    ret = send(s, data, (int) numbytes, flags);

#ifdef _WIN32
    if (ret == -1) err = WSAGetLastError();
    leave_blocking_section();
    if (ret == -1) win32_maperr(err);
#else
    leave_blocking_section();
#endif

    if (ret == -1) uerror("mem_send", Nothing);
    return Val_long(ret);
}


/**********************************************************************/
/* Bigarrays as value buffers                                         */
/**********************************************************************/

CAMLprim value netsys_as_value(value memv, value offv) 
{
    struct caml_bigarray *b = Bigarray_val(memv);
    return (value) (b->data + Long_val(offv));
}

CAMLprim value netsys_cmp_string(value s1, value s2)
{
    mlsize_t l1, l2, k;
    unsigned char *c1, *c2;
    if (s1 == s2) return Val_int(0);
    l1 = caml_string_length(s1);
    l2 = caml_string_length(s2);
    c1 = (unsigned char *) String_val(s1);
    c2 = (unsigned char *) String_val(s2);
    k = 0;
    while (k < l1 && k < l2) {
	if (*c1 != *c2) 
	    return Val_int( (int) *c1 - (int) *c2 );
	c1++;
	c2++;
	k++;
    }
    return Val_long ( l1 - l2 );
}


CAMLprim value netsys_init_string(value memv, value offv, value lenv) 
{
    struct caml_bigarray *b = Bigarray_val(memv);
    long off = Long_val(offv);
    long len = Long_val(lenv);
    value *m;
    char *m_b;
    mlsize_t wosize;
    mlsize_t offset_index;

#ifdef ARCH_SIXTYFOUR
    if (off % 8 != 0)
	invalid_argument("Netsys_mem.init_string");
#else
    if (off % 4 != 0)
	invalid_argument("Netsys_mem.init_string");
#endif

    m = (value *) (((char *) b->data) + off);
    m_b = (char *) m;
    wosize = (len + sizeof (value)) / sizeof (value);  /* >= 1 */
    
    m[0] = /* Make_header (wosize, 0, Caml_black) */
	(value) (((header_t) wosize << 10) + (3 << 8));
    m[wosize] = 0;

    offset_index = Bsize_wsize (wosize) - 1;
    m_b[offset_index + sizeof(value)] = offset_index - len;

    return Val_unit;
}


int netsys_init_value_1(struct htab *t,
			struct queue *q,
			value memv, 
			value offv, 
			value orig,  
			int enable_bigarrays, 
			int enable_customs,
			int enable_atoms,
			int simulation,
			void *target_addr,
			long *start_offset,
			long *bytelen
			)
{
    void *orig_addr;
    void *work_addr;
    value work;
    int   work_tag;
    char *work_header;
    size_t work_bytes;
    size_t work_words;
    void *copy_addr;
    value copy;
    char *copy_header;
    header_t copy_header1;
    int   copy_tag;
    size_t copy_words;
    void *fixup_addr;
    char *mem_data;
    char *mem_cur;
    char *mem_end;
    char *mem_ptr;
    long  off;
    int code, i;
    intnat addr_delta;

    copy = 0;

    off = Long_val(offv);
    mem_data = (char *) Bigarray_val(memv)->data;
    mem_end = mem_data + Bigarray_val(memv)->dim[0];
    mem_cur = mem_data + off;
    addr_delta = ((char *) target_addr) - mem_data;

    if (mem_cur >= mem_end && !simulation) return (-4);   /* out of space */

    if (!Is_block(orig)) return (-2);
    if (off % sizeof(void *) != 0) return (-2);

    orig_addr = (void *) orig;
    code = netsys_queue_add(q, orig_addr);
    if (code != 0) return code;

    /* First pass: Iterate over the addresses found in q. Ignore
       addresses already seen in the past (which are in t). For
       new addresses, make a copy, and add these copies to t.
    */

    fprintf(stderr, "first pass, orig_addr=%lx simulation=%d addr_delta=%lx\n",
       (unsigned long) orig_addr, simulation, addr_delta);

    code = netsys_queue_take(q, &work_addr);
    while (code != (-3)) {
	if (code != 0) return code;

	/* fprintf(stderr, "work_addr=%lx\n", (unsigned long) work_addr); */

	code = netsys_htab_lookup(t, work_addr, &copy_addr);
	if (code != 0) return code;

	if (copy_addr == NULL) {
	    /* The address is unknown, so copy the value */

	    /* Body of first pass */
	    work = (value) work_addr;
	    work_tag = Tag_val(work);
	    work_header = Hp_val(work);
	    
	    if (work_tag < No_scan_tag) {
		/* It is a scanned value (with subvalues) */
		
		switch(work_tag) {
		case Object_tag:
		case Closure_tag:
		case Lazy_tag:
		case Forward_tag:
		    return (-2);   /* unsupported */
		}

		work_words = Wosize_hp(work_header);
		if (work_words == 0 && !enable_atoms) return (-2);
		
		/* Do the copy. */

		work_bytes = Bhsize_hp(work_header);
		copy_header = mem_cur;
		mem_cur += work_bytes;
		if (mem_cur > mem_end && !simulation) return (-4);
		
		if (simulation) 
		    copy_addr = work_addr;
		else {
		    memcpy(copy_header, work_header, work_bytes);
		    copy = Val_hp(copy_header);
		    copy_addr = (void *) copy;
		}

		/* Add the association (work_addr -> copy_addr) to t: */

		code = netsys_htab_add(t, work_addr, copy_addr);
		if (code < 0) return code;

		/* Add the sub values of work_addr to q: */

		for (i=0; i < work_words; ++i) {
		    value field = Field(work, i);
		    if (Is_block (field)) {
			code = netsys_queue_add(q, (void *) field);
			if (code != 0) return code;
		    }
		}
	    }
	    else {
		/* It an opaque value */
		int do_copy = 0;
		int do_bigarray = 0;
		/* Check for bigarrays and other custom blocks */
		switch (work_tag) {
		case Abstract_tag:
		    return(-2);
		case String_tag:
		    do_copy = 1; break;
		case Double_tag:
		    do_copy = 1; break;
		case Double_array_tag:
		    do_copy = 1; break;
		case Custom_tag: 
		    {
			struct custom_operations *custom_ops;
			char *id;

			custom_ops = Custom_ops_val(work);
			id = custom_ops->identifier;
			if (id[0] == '_') {
			    switch (id[1]) {
			    case 'b':
				if (strcmp(id, "_bigarray") == 0) {
				    if (enable_bigarrays) {
					struct caml_ba_array *b;
					int m;
					b = Bigarray_val(work);
					m = b->flags & CAML_BA_MANAGED_MASK;
					/* Only non-mapped O'Caml-managed arrays: */
					if (m != CAML_BA_MANAGED)
					    return (-2);
					/* There must not be any proxies, i.e. this
					   bigarray must be neither a subarray, or
					   a superarray
					*/
					if (b->proxy != NULL) 
					    return (-2);
					do_copy = 1;
					do_bigarray = 1;
				    }
				    else
					return (-2);
				};
				break;
			    case 'i': /* int32 */
			    case 'j': /* int64 */
			    case 'n': /* nativeint */
				if (!enable_customs) return (-2);
				if (id[2] == 0) {
				    do_copy = 1;
				    break;
				}
			    default:
				return (-2);
			    }
			}
			else
			    return (-2);
		    }
		} /* switch */

		if (do_copy) {  
		    /* Copy the value */
		    work_bytes = Bhsize_hp(work_header);
		    copy_header = mem_cur;
		    mem_cur += work_bytes;

		    if (simulation)
			copy_addr = work_addr;
		    else {
			if (mem_cur > mem_end) return (-4);
			memcpy(copy_header, work_header, work_bytes);
			copy = Val_hp(copy_header);
			copy_addr = (void *) copy;
		    }
		    
		    code = netsys_htab_add(t, work_addr, copy_addr);
		    if (code < 0) return code;
		}

		if (do_bigarray) {
		    /* postprocessing for copying bigarrays */
		    struct caml_ba_array *b_work, *b_copy;
		    void * data_copy;
		    char * data_header;
		    header_t data_header1;
		    size_t size = 1;
		    size_t size_aligned;
		    b_work = Bigarray_val(work);
		    b_copy = Bigarray_val(copy);
		    for (i = 0; i < b_work->num_dims; i++) {
			size = size * b_work->dim[i];
		    };
		    size = 
			size * 
			caml_ba_element_size[b_work->flags & BIGARRAY_KIND_MASK];

		    size_aligned = size;
		    if (size%sizeof(void *) != 0)
			size_aligned += sizeof(void *) - (size%sizeof(void *));

		    /* Also allocate mem for a header: */
		    
		    data_header = mem_cur;
		    data_copy = mem_cur + sizeof(void *);
		    mem_cur += size_aligned + sizeof(void *);

		    if (!simulation) {
			if (mem_cur > mem_end) return (-4);

			/* Initialize header: */
			
			data_header1 =
			    (size_aligned << 10) + (3 << 8) + Abstract_tag;
			memcpy(data_header, (char *) &data_header1,
			       sizeof(header_t));

			/* Copy bigarray: */
			
			memcpy(data_copy, b_work->data, size);
			b_copy->data = data_copy;
		    }
		}

	    } /* if (work_tag < No_scan_tag) */
	} /* if (copy_addr == NULL) */

	/* Switch to next address in q: */

	code = netsys_queue_take(q, &work_addr);
    } /* while */
    
    /* Second pass. The copied blocks still have fields pointing to the
       original blocks. We fix that now by iterating once over the copied
       memory block.
    */

    if (!simulation) {
	fprintf(stderr, "second pass\n");
	mem_ptr = mem_data + off;
	while (mem_ptr < mem_cur) {
	    copy_header1 = *((header_t *) mem_ptr);
	    copy_tag = Tag_hd(copy_header1);
	    copy_words = Wosize_hd(copy_header1);
	    copy = (value) (mem_ptr + sizeof(void *));
	    
	    if (copy_tag < No_scan_tag) {
		for (i=0; i < copy_words; ++i) {
		    value field = Field(copy, i);
		    if (Is_block (field)) {
			/* It is a pointer. Try to fix it up. */
			code = netsys_htab_lookup(t, (void *) field,
						  &fixup_addr);
			if (code != 0) return code;
			
			Field(copy,i) = 
			    (value) (((char *) fixup_addr) + addr_delta);
		    }
		}
	    }
	    
	    mem_ptr += (copy_words + 1) * sizeof(void *);
	}
    }	

    /* hey, fine. Return result */
    *start_offset = off + sizeof(void *);
    *bytelen = mem_cur - mem_data - off;

    /* fprintf(stderr, "return regularly\n");*/

    return 0;
}


static int init_value_flags[] = { 1, 2, 4, 8 };

value netsys_init_value(value memv, 
			value offv, 
			value orig,  
			value flags,
			value targetaddrv
			)
{
    int code;
    struct htab t;
    int         t_init;
    struct queue q;
    int          q_init;
    value r;
    long start_offset, bytelen;
    int  cflags;
    void *targetaddr;
    
    q_init=0;
    t_init=0;

    cflags = caml_convert_flag_list(flags, init_value_flags);
    targetaddr = (void *) Nativeint_val(targetaddrv);

    code = netsys_queue_init(&q, 1000);
    if (code != 0) goto exit;
    q_init=1;
    
    code = netsys_htab_init(&t, 1000);
    if (code != 0) goto exit;
    t_init=1;

    code = netsys_init_value_1(&t, &q, memv, offv, orig, 
			       cflags & 1, cflags & 2, cflags & 4, cflags & 8,
			       targetaddr, &start_offset, &bytelen);
    if (code != 0) goto exit;

    netsys_queue_free(&q);
    q_init = 0;
    netsys_htab_free(&t);
    t_init = 0;
    
    r = caml_alloc_small(2,0);
    Field(r,0) = Val_long(start_offset);
    Field(r,1) = Val_long(bytelen);

    return r;

 exit:
    if (q_init) netsys_queue_free(&q);
    if (t_init) netsys_htab_free(&t);

    switch(code) {
    case (-1):
	unix_error(errno, "netsys_init_value", Nothing);
    case (-2):
	failwith("Netsys_mem.init_value: Library error");
    case (-4):
	caml_raise_constant(*caml_named_value("Netsys_mem.Out_of_space"));
    default:
	failwith("Netsys_mem.init_value: Unknown error");
    }
}
