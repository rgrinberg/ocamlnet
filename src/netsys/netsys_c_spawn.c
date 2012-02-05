/* $Id$ */

#include "netsys_c.h"

#ifdef HAVE_PTHREAD
#include <pthread.h>
#endif


/**********************************************************************/
/* spawn                                                              */
/**********************************************************************/

static void empty_signal_handler(int sig) {}

typedef union {
    char buffer[256];
    struct {
	int   b_errno;
	char  b_function[64];
    } decoded;
} marshalled_error;


#define MAIN_ERROR(e,f) { uerror_errno = e; uerror_function = f; goto main_exit; }
#define SUB_ERROR(e,f) { uerror_errno = e; uerror_function = f; goto sub_error; }



/* Note: In the following function we can assume that we are not on Win32.
   Hence, file descriptors are simply ints.
*/

CAMLprim value netsys_spawn_nat(value v_chdir,
				value v_pg,
				value v_fd_actions,
				value v_sig_actions,
				value v_env,
				value v_cmd,
				value v_args) {
#ifdef HAVE_FORK_EXEC
    int   uerror_errno;
    char *uerror_function;
    value return_value;

    int code;
    sigset_t mask;
    sigset_t save_mask;
    int   cleanup_mask;

    int ctrl_pipe[2];
    int cleanup_pipe0;
    int cleanup_pipe1;

    int cleanup_bsection;

    pid_t pid;
    char **sub_argv;
    char **sub_env;
    int cleanup_sub_argv;
    int cleanup_sub_env;;

    marshalled_error me;
    ssize_t n;

    char *ttyname;
    int   ttyfd;

    struct sigaction sigact;
    int signr;

    value v_sig_actions_l;
    value v_sig_actions_hd;

    value v_fd_actions_l;
    value v_fd_actions_hd;
    value v_fd_actions_0;

    int j, k, l;
    int fd1, fd2, fd1_flags;

    uerror_errno = 0;
    cleanup_mask = 0;
    cleanup_pipe0 = 0;
    cleanup_pipe1 = 0;
    cleanup_bsection = 0;
    cleanup_sub_argv = 0;
    cleanup_sub_env = 0;

    sub_argv = NULL;
    sub_env = NULL;
    return_value = Val_int(0);
    uerror_function = "<uninit>";

    /* First thing is that we have to block all signals. In a multi-threaded
       program this is only done for the thread calling us, otherwise for the
       whole process. [fork] will reset all pending signals, so we can be
       sure the subprocess won't get any signals until we perform the
       signal actions in the subprocess.

       In the calling process, the mask is reset below at [exit].
    */
    code = sigfillset(&mask);
    if (code == -1) unix_error(EINVAL, "netsys_spawn/sigfillset [000]", 
			       Nothing);
#ifdef HAVE_PTHREAD
    code = pthread_sigmask(SIG_SETMASK, &mask, &save_mask);
    if (code != 0) unix_error(code, "netsys_spawn/pthread_sigmask [001]", 
			      Nothing);
#else
#ifdef HAVE_POSIX_SIGNALS
    code = sigprocmask(SIG_SETMASK, &mask, &save_mask);
    if (code == -1) uerror("netsys_spawn/sigprocmask [002]", Nothing);
#endif
#endif

    /* From now on, we don't jump out with uerror, but leave via "exit" 
       below.
    */
    cleanup_mask = 1;

    /* Create the control pipe for reporting errors. */
    code = pipe(ctrl_pipe);
    if (code == -1) MAIN_ERROR(errno, "netsys_spawn/pipe [010]");
    cleanup_pipe0 = 1;
    cleanup_pipe1 = 1;

    /* Prepare sub_argv and sub_env: */
    sub_argv = malloc((Wosize_val(v_args) + 1) * sizeof(char *));
    if (sub_argv == NULL) MAIN_ERROR(ENOMEM, "netsys_spawn/malloc [020]");
    for (k = 0; k < Wosize_val(v_args); k++) {
	sub_argv[k] = String_val(Field(v_args, k));
    }
    sub_argv[ Wosize_val(v_args)] = NULL;
    cleanup_sub_argv = 1;

    sub_env = malloc((Wosize_val(v_env) + 1) * sizeof(char *));
    if (sub_env == NULL) MAIN_ERROR(ENOMEM, "netsys_spawn/malloc [021]");
    for (k = 0; k < Wosize_val(v_env); k++) {
	sub_env[k] = String_val(Field(v_env, k));
    }
    sub_env[ Wosize_val(v_env)] = NULL;
    cleanup_sub_env = 1;

    /* Because fork() can be slow we allow here that other threads can run */
    /* caml_enter_blocking_section();
       cleanup_bsection = 1;
       -- TODO: check this more carefully before enabling it
       -- see also leave_blocking_section below
    */

    /* Fork the process. */
    pid = fork();
    if (pid == (pid_t) -1) 
	MAIN_ERROR(errno, "netsys_spawn/fork [031]");
    if (pid != (pid_t) 0)
	goto main_process;

/*sub_process:*/
    /* The start of the sub process. */
    pid = getpid();

    /* Close the read side of the control pipe. Set the close-on-exec flag
       for the write side.
     */
    code = close(ctrl_pipe[0]);
    if (code == -1) SUB_ERROR(errno, "netsys_spawn/close [100]");
    code = fcntl(ctrl_pipe[1], F_SETFD, FD_CLOEXEC);
    if (code == -1) SUB_ERROR(errno, "netsys_spawn/fcntl [101]");

    /* If required, change the working directory */
    if (Is_block(v_chdir)) {
	switch(Tag_val(v_chdir)) {
	case 0:  /* Wd_chdir */
	    code = chdir(String_val(Field(v_chdir, 0)));
	    if (code == -1) SUB_ERROR(errno, "netsys_spawn/chdir [110]");
	    break;

	case 1:  /* Wd_fchdir */
	    code = fchdir(Int_val(Field(v_chdir, 1)));
	    if (code == -1) SUB_ERROR(errno, "netsys_spawn/fchdir [111]");
	    break;

	default:
	    SUB_ERROR(EINVAL, "netsys_spawn/assert_chdir [112]");
	}
    }

    /* If required, create/join the process group */
    if (Is_block(v_pg)) {
	/* Must be Pg_join_group */
#ifdef HAVE_POSIX_PROCESS_GROUPS
	code = setpgid(0, Int_val(Field(v_pg, 0)));
	if (code == -1) SUB_ERROR(errno, "netsys_spawn/setpgid [120]");
#endif	
    }
    else {
	switch (Int_val(v_pg)) {
	case 0: /* Pg_keep */
	    break;
	case 1: /* Pg_new_bg_group */
#ifdef HAVE_POSIX_PROCESS_GROUPS
	    code = setpgid(0, 0);
	    if (code == -1) SUB_ERROR(errno, "netsys_spawn/setpgid [130]");
#endif	
	    break;
	case 2: /* Pg_new_fg_group */
#ifdef HAVE_POSIX_TTY
	    code = setpgid(0, 0);
	    if (code == -1) SUB_ERROR(errno, "netsys_spawn/setpgid [140]");
	    ttyname = ctermid(NULL);   /* no error code defined! */
	    ttyfd = open(ttyname, O_RDWR);
	    if (ttyfd == -1) SUB_ERROR(errno, "netsys_spawn/open [141]");
	    /* tcsetpgrp may send a SIGTTOU signal to this process. We want
               to hide this signal, so we set this signal to be ignored.
               We do this by setting an empty signal handler. On exec,
               the SIGTTOU signal will be reset to the default action in
               this case - which is ok because the set of pending signals
               is also cleared.
	    */
	    sigact.sa_sigaction = NULL;
	    sigact.sa_handler = &empty_signal_handler;
	    sigact.sa_flags = 0;
	    code = sigemptyset(&(sigact.sa_mask));
	    if (code == -1) SUB_ERROR(EINVAL, "netsys_spawn/sigemptyset [142]");
	    code = sigaction(SIGTTOU, &sigact, NULL);
	    if (code == -1) SUB_ERROR(errno, "netsys_spawn/sigaction [143]");
	    code = tcsetpgrp(ttyfd, pid);
	    if (code == -1) SUB_ERROR(errno, "netsys_spawn/tcsetpgrp [144]");
	    code = close(ttyfd);
	    if (code == -1) SUB_ERROR(errno, "netsys_spawn/close [145]");
#endif
	    break;
	default:
	    SUB_ERROR(EINVAL, "netsys_spawn/assert_pg [160]");
	}
    }

    /* do the signal stuff: */
#ifdef HAVE_POSIX_SIGNALS
    v_sig_actions_l = v_sig_actions;
    while (Is_block(v_sig_actions_l)) {
	v_sig_actions_hd = Field(v_sig_actions_l, 0);
	v_sig_actions_l  = Field(v_sig_actions_l, 1);
	switch(Tag_val(v_sig_actions_hd)) {
	case 0:  /* Sig_default */
	    signr = caml_convert_signal_number
	  	       (Int_val(Field(v_sig_actions_hd,0)));
	    sigact.sa_sigaction = NULL;
	    sigact.sa_handler = SIG_DFL;
	    sigact.sa_flags = 0;
	    code = sigemptyset(&(sigact.sa_mask));
	    if (code == -1) SUB_ERROR(EINVAL, "netsys_spawn/sigemptyset [170]");
	    code = sigaction(signr, &sigact, NULL);
	    if (code == -1) SUB_ERROR(errno, "netsys_spawn/sigaction [171]");
	    break;
	case 1: /* Sig_ignore */
	    signr = caml_convert_signal_number
	  	       (Int_val(Field(v_sig_actions_hd,0)));
	    sigact.sa_sigaction = NULL;
	    sigact.sa_handler = SIG_IGN;
	    sigact.sa_flags = 0;
	    code = sigemptyset(&(sigact.sa_mask));
	    if (code == -1) SUB_ERROR(EINVAL, "netsys_spawn/sigemptyset [180]");
	    code = sigaction(signr, &sigact, NULL);
	    if (code == -1) SUB_ERROR(errno, "netsys_spawn/sigaction [181]");
	    break;
	default:
	    SUB_ERROR(EINVAL, "netsys_spawn/assert_sig [190]");
	}
    };
#endif

    /* do the fd stuff: */
    v_fd_actions_l = v_fd_actions;
    while (Is_block(v_fd_actions_l)) {
	v_fd_actions_hd = Field(v_fd_actions_l, 0);
	v_fd_actions_l  = Field(v_fd_actions_l, 1);
	switch(Tag_val(v_fd_actions_hd)) {
	case 0: /* Fda_close */
	    fd1 = Int_val(Field(v_fd_actions_hd, 0));
	    if (fd1 == ctrl_pipe[1]) 
		SUB_ERROR(EBADF, "netsys_spawn/fda_close [200]");
	    code = close(fd1);
	    if (code == -1) SUB_ERROR(errno, "netsys_spawn/close [201]");
	    break;
	case 1: /* Fda_close_ignore */
	    fd1 = Int_val(Field(v_fd_actions_hd, 0));
	    if (fd1 != ctrl_pipe[1]) {
		code = close(fd1);
		if (code == -1 && errno != EBADF)
		    SUB_ERROR(errno, "netsys_spawn/close [210]");
	    }  
	    /* ignore requests to close the ctrl_pipe, it's closed
	       anyway later 
	    */
	    break;
	case 2:  /* Fda_close_except */
	    v_fd_actions_0 = Field(v_fd_actions_hd, 0);
	    j = Wosize_val(v_fd_actions_0);   /* array length */
	    l = sysconf(_SC_OPEN_MAX);
	    for (k=0; k<l; k++) {
		if (k>=j || !Bool_val(Field(v_fd_actions_0,k))) {
		    if (k != ctrl_pipe[1])
			close(k);   /* ignore any error */
		}
	    }
	    break;
	case 3: /* Fda_dup2 */
	    fd1 = Int_val(Field(v_fd_actions_hd, 0));
	    fd2 = Int_val(Field(v_fd_actions_hd, 1));
	    /* If fd1 is the ctrl_pipe, return EBADF: */
	    if (fd1 == ctrl_pipe[1]) 
		SUB_ERROR(EBADF, "netsys_spawn/fda_dup2 [220]");
	    /* Check that fd1 is valid by reading the fd flags: */
	    code = fcntl(fd1, F_GETFD);
	    if (code == -1) SUB_ERROR(errno, "netsys_spawn/fcntl [221]");
	    fd1_flags = code;
	    /* Be careful when fd2 is the ctrl_pipe: */
	    if (fd2 == ctrl_pipe[1]) {
		code = dup(ctrl_pipe[1]);
		if (code == -1) SUB_ERROR(errno, "netsys_spawn/dup [222]");
		ctrl_pipe[1] = code;
		code = fcntl(ctrl_pipe[1], F_SETFD, FD_CLOEXEC);
		if (code == -1) SUB_ERROR(errno, "netsys_spawn/fcntl [223]");
	    }
	    code = dup2(fd1, fd2);
	    if (code == -1) SUB_ERROR(errno, "netsys_spawn/dup2 [224]");
	    /* The FD_CLOEXEC flag remains off for the duped descriptor */
	    break;
	default:
	    SUB_ERROR(EINVAL, "netsys_spawn/assert_fd [230]");
	};
    };

    /* reset the signal mask: */
#ifdef HAVE_POSIX_SIGNALS
    code = sigprocmask(SIG_SETMASK, &save_mask, NULL);
    if (code == -1) SUB_ERROR(errno, "netsys_spawn/sigprocmask [241]");
#endif

    /* exec the new program: */
    code = execve(String_val(v_cmd),
		  sub_argv,
		  sub_env);
    if (code == -1) SUB_ERROR(errno, "netsys_spawn/execve [290]");

    SUB_ERROR(EINVAL, "netsys_spawn/assert_execve [291]");

 sub_error:
    /* Marshal the error in uerror_errno and uerror_function */
    me.decoded.b_errno = uerror_errno;
    strcpy(me.decoded.b_function, uerror_function);
    
    n = write(ctrl_pipe[1], me.buffer, sizeof(me.buffer));
    /* it doesn't make much sense here to check for write errors */

    _exit(127);
    
 main_process:
    /* Here the main process continues after forking. There's not much to do
       here: We close the write side of the control pipe, so the read side
       can see EOF. We check then whether the read side is just closed
       (meaning no error), or whether there are bytes, the marshalled
       error condition.
    */
    if (cleanup_bsection) {
	caml_leave_blocking_section();
	cleanup_bsection = 0;
    };

    code = close(ctrl_pipe[1]);
    if (code == -1) {
	uerror_errno = errno;
	uerror_function = "netsys_spawn/close [300]";
	goto main_exit;
    };
    cleanup_pipe1 = 0;   /* it's already closed */

    n = read(ctrl_pipe[0], me.buffer, sizeof(me.buffer));
    if (n == (ssize_t) -1) {
	uerror_errno = errno;
	uerror_function = "netsys_spawn/read [301]";
	goto main_exit;
    };

    if (n == 0) {
	/* hey, we have success! */
	return_value = Val_int(pid);
	goto main_exit;
    }
    
    /* There is an error message in me. Look at it. */
    if (n != (ssize_t) sizeof(me.buffer)) {
	uerror_errno = EINVAL;
	uerror_function = "netsys_spawn/assert_me [302]";
    }

    /* Also don't forget to wait on the child to avoid zombies: */
    code = 1;
    while (code) {
	code = waitpid(pid, NULL, 0);
	code = (code == -1 && errno == EINTR);
    };

    uerror_errno = me.decoded.b_errno;
    uerror_function = me.decoded.b_function;
    /* now exit... */

main_exit:
    /* Policy: If we already have an error to report, and any of the
       cleanup actions also indicates an error, we return the first
       error to the caller.
    */
    
    if (cleanup_bsection)
	caml_leave_blocking_section();

    if (cleanup_mask) {
#ifdef HAVE_PTHREAD
	code = pthread_sigmask(SIG_SETMASK, &save_mask, NULL);
	if (code != 0 && uerror_errno == 0) {
	    uerror_errno = code;
	    uerror_function = "netsys_spawn/pthread_sigmask [400]";
	}
#else
#ifdef HAVE_POSIX_SIGNALS
	code = sigprocmask(SIG_SETMASK, &save_mask, NULL);
	if (code == -1 && uerror_errno == 0) {
	    uerror_errno = errno;
	    uerror_function = "netsys_spawn/sigprocmask [401]";
	}
#endif
#endif
    };

    if (cleanup_pipe0) {
	code = close(ctrl_pipe[0]);
	if (code == -1 && uerror_errno == 0) {
	    uerror_errno = errno;
	    uerror_function = "netsys_spawn/close [410]";
	}
    }

    if (cleanup_pipe1) {
	code = close(ctrl_pipe[1]);
	if (code == -1 && uerror_errno == 0) {
	    uerror_errno = errno;
	    uerror_function = "netsys_spawn/close [411]";
	}
    }

    if (cleanup_sub_argv) {
	free(sub_argv);
    }

    if (cleanup_sub_env) {
	free(sub_env);
    }

    if (uerror_errno != 0)
	unix_error(uerror_errno, uerror_function, Nothing);

    return return_value;
#else
     invalid_argument("netsys_spawn");
#endif
}


CAMLprim value netsys_spawn_byte(value * argv, int argn) 
{
    return netsys_spawn_nat(argv[0], argv[1], argv[2], argv[3],
			    argv[4], argv[5], argv[6]);
}

#undef MAIN_ERROR
#undef SUB_ERROR
