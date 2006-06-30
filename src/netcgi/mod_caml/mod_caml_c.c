/* mod_caml
 * Copyright (C) 2003 Merjis Ltd.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the Free
 * Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 * $Id: mod_caml_c.c,v 1.12 2005/01/25 18:36:35 ChriS Exp $
 */

#include "config.h"

#include <stdio.h>
#include <time.h>
#include <unistd.h>

#include <assert.h>

#include <caml/alloc.h>
#include <caml/callback.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>

#include <httpd.h>
#include <http_config.h>
#if APACHE2
#include <http_request.h>
#include <http_protocol.h>
#endif

#include "wrappers.h"

#if APACHE2
#define pool apr_pool_t
#define ap_palloc apr_palloc
#endif

#define VERSION_STRING PACKAGE "/" VERSION

/* XXX Temporary fix for ocaml 3.08.0 which doesn't prototype this symbol. */
#if 1
CAMLextern char * format_caml_exception(value exn);
#endif

extern module caml_module;

/* The module_init hook isn't called until after configuration. Duh!
 * So we need something which really _will_ be called early, to
 * initialize the OCaml runtime.
 */
static void init (void) __attribute__((constructor));
#pragma init(init)

static void
init (void)
{
  static char *argv[] = {
    APACHELIBDIR "/mod_caml.so",
    APACHELIBDIR "/mod_caml.so",
    NULL
  };

  /* Remove a possible source of error by bailing if argv[0] doesn't exist. */
  if (access (argv[0], R_OK) == -1)
    {
      perror (argv[0]);
      fprintf (stderr,
	       "This probably means one of several things:\n"
	       "* APACHELIBDIR wasn't set correctly in Makefile.config\n"
	       "* Program has not been installed.\n"
	       "* File has been moved/removed since installation.\n"
	       "In any case you must fix this before mod_caml can run.\n");
      abort ();
    }

  /* Start the OCaml bytecode interpreter. */
  caml_main (argv);
}

#if APACHE2
static int
post_config (apr_pool_t *pPool, apr_pool_t *pLog, apr_pool_t *pTemp,
	     server_rec *s)
{
  ap_add_version_component (pPool, VERSION_STRING);
  static value *f = NULL;

  if(f == NULL) f = caml_named_value("mod_caml_module_init");
  callback(*f, Val_unit);
  return OK;
}
#else
static void
module_init (server_rec *s, pool *p)
{
  ap_add_version_component (VERSION_STRING);
  static value *f = NULL;

  if(f == NULL) f = caml_named_value("mod_caml_module_init");
  callback(*f, Val_unit);
}
#endif

/* Notes on per-server and per-directory configuration.
 *
 * The actual OCaml data that we store here is defined in the immutable
 * structs Mod_caml.server_config_t and Mod_caml.dir_config_t.  You are
 * advised to go and look at those now ...
 *
 * Apache lets us store a pointer (void *) and leaves it up to us
 * what this actually points to.  In our case the pointer (v in the
 * diagram) points to an OCaml value.  The OCaml value is presumed
 * to be a pointer to the structure itself.
 *
 *              4/8 bytes
 *              in pool            allocated on OCaml heap
 *             +----------+       +-- the structure ---------+
 *    v ---->  | value   -------> | either server_config_t   |
 *             +----------+       | or dir_config_t          |
 *                                | ...                      |
 *                                | ...                      |
 *                                | ...                      |
 *                                +--------------------------+
 *
 * The value is allocated in the pool which Apache gives us.  This
 * pool should usually last for the lifetime of the server, but it's
 * up to Apache to sort that out.
 *
 * In addition, we make the value be an OCaml GC global root.  This
 * means that the GC won't free up the structure prematurely.
 *
 * Also we register a cleanup function in the pool.  When Apache does
 * finally deallocate the pool, our cleanup function will remove the
 * global root.
 *
 * Note that the structure is immutable, so, for example,
 * cmd_translate_handler and friends actually create a new structure
 * and update the value to point to the new structure.
 */

/* Allocate a global root value in the pool, and register a cleanup which
 * will be called when the pool dies to unregister the global root.
 */
#if APACHE2
static int
remove_root_value (void *vp)
{
  remove_global_root ((value *) vp);
  return OK;
}
#endif

static value *
alloc_root_value (pool *p)
{
  value *v = (value *) ap_palloc (p, sizeof (value));
  *v = Val_unit;
  register_global_root (v);
#if APACHE2
  apr_pool_cleanup_register (p, v, remove_root_value, apr_pool_cleanup_null);
#else
  ap_register_cleanup (p, v,
		       (void (*)(void *)) remove_global_root, ap_null_cleanup);
#endif
  return v;
}

static void *
create_dir_config (pool *p, char *dirname)
{
  value arg = Val_optstring (dirname);
  value *v = alloc_root_value (p);
  static value *f = NULL;

  if(f == NULL) f = caml_named_value("mod_caml_create_dir_config");
  *v = callback(*f, arg);
  return v;
}

static void *
merge_dir_config (pool *p, void *base, void *add)
{
  value *v = alloc_root_value (p);
  static value *f = NULL;

  if(f == NULL) f = caml_named_value("mod_caml_merge_dir_config");
  *v = callback2(*f, *(value *) base, *(value *) add);
  return v;
}

static void *
create_server_config (pool *p, server_rec *s)
{
  value arg = Val_server_rec (s);
  value *v = alloc_root_value (p);
  static value *f = NULL;

  if(f == NULL) f = caml_named_value("mod_caml_create_server_config");
  *v = callback(*f, arg);
  return v;
}

static void *
merge_server_config (pool *p, void *base, void *add)
{
  value *v = alloc_root_value (p);
  static value *f = NULL;

  if(f == NULL) f = caml_named_value("mod_caml_merge_server_config");
  *v = callback2(*f, *(value *) base, *(value *) add);
  return v;
}


/* Turn HttpError exception into the appropriate return code. */
static int
exception_in_handler (value exn, const char *function_name)
{
  char strtime[26];
  time_t secs;
  static value *http_error_exn = NULL;
  if (http_error_exn == NULL)
    http_error_exn = caml_named_value("mod_caml_http_error");

  if (Field(exn, 0) == *http_error_exn) {
    int status_code = Int_val(Field(exn, 1));
    /* fprintf(stderr, "%s: HTTP %d\n", function_name, status_code); */
    return status_code;
  }
  /* Unknown other exception: log it and return internal server error. */
  strtime[0] = '\0';
  time(&secs);
  ctime_r(&secs, strtime);
  strtime[24] = '\0'; /* Remove the '\n' put by ctime */
  fprintf(stderr, "[%s] [Mod_caml] %s: Uncaught exception \"%s\".\n",
	  strtime, function_name, caml_format_exception(exn));
  return HTTP_INTERNAL_SERVER_ERROR;
}

#define MAKE_HANDLER(name)					\
static int							\
name (request_rec *r)						\
{								\
  static value *f = NULL;					\
  value rv, arg;						\
  if (f == NULL) f = caml_named_value("mod_caml_" #name);	\
  assert(f);							\
  arg = Val_request_rec(r);					\
  rv = callback_exn(*f, arg);					\
  if (Is_exception_result (rv))					\
    return exception_in_handler (Extract_exception (rv), __func__); \
  return Result_type_val (rv);					\
}

MAKE_HANDLER (translate_handler)
MAKE_HANDLER (check_user_id)
MAKE_HANDLER (auth_checker)
MAKE_HANDLER (access_checker)
MAKE_HANDLER (type_checker)
MAKE_HANDLER (fixer_upper)
MAKE_HANDLER (logger)
MAKE_HANDLER (header_parser)
MAKE_HANDLER (post_read_request)
MAKE_HANDLER (ocaml_bytecode_handler)

static const char *
cmd_load (cmd_parms *parms, void *dummy, const char *filename)
{
  static value *f = NULL;
  value arg = copy_string (filename);
  value exn;
  if (f == NULL) f = caml_named_value("mod_caml_cmd_load");
  exn = callback_exn(*f, arg);
  if (Is_exception_result (exn))
    /* XXX ap_pstrdup into pool? */
    return caml_format_exception (Extract_exception (exn));
  return NULL;
}

/* Deprecated: does nothing. */
static const char *
cmd_add_interface (cmd_parms *parms, void *dummy, const char *name)
{
  return NULL;
}

/* Deprecated: does nothing. */
static const char *
cmd_add_interface_path (cmd_parms *parms, void *dummy, const char *path)
{
  return NULL;
}

static const char *
cmd_translate_handler (cmd_parms *parms, void *dummy, const char *name)
{
  value *sconfig = (value *)
    ap_get_module_config (parms->server->module_config, &caml_module);
  static value *f = NULL;
  value arg = copy_string(name);
  value exn;
  if (f == NULL) f = caml_named_value("mod_caml_cmd_translate_handler");
  exn = callback2_exn(*f, *sconfig, arg);
  if (Is_exception_result (exn))
    /* XXX ap_pstrdup into pool? */
    return caml_format_exception (Extract_exception (exn));
  *sconfig = exn;		/* Update server config. */
  return NULL;
}

#define MAKE_CMD(name)						\
static const char *						\
name (cmd_parms *parms, void *dconfig, const char *name)	\
{								\
  value *f = NULL;						\
  value arg = copy_string(name);				\
  value exn;							\
  if (f == NULL) f = caml_named_value("mod_caml_" #name);	\
  exn = callback2_exn(*f, *(value *) dconfig, arg);		\
  if (Is_exception_result (exn))				\
    /* XXX ap_pstrdup into pool? */				\
    return caml_format_exception (Extract_exception (exn));	\
  *(value *)dconfig = exn;	/* Update dir config. */	\
  return NULL;							\
}

MAKE_CMD (cmd_check_user_id_handler)
MAKE_CMD (cmd_auth_checker_handler)
MAKE_CMD (cmd_access_checker_handler)
MAKE_CMD (cmd_type_checker_handler)
MAKE_CMD (cmd_fixer_upper_handler)
MAKE_CMD (cmd_logger_handler)
MAKE_CMD (cmd_header_parser_handler)
MAKE_CMD (cmd_post_read_request_handler)
MAKE_CMD (cmd_handler)

#if APACHE2

static command_rec cmds[] = {
  AP_INIT_TAKE1 ("CamlLoad", cmd_load,
		 NULL,
		 RSRC_CONF,
		 "load OCaml module"),
  AP_INIT_TAKE1 ("CamlAddInterface", cmd_add_interface,
		 NULL,
		 RSRC_CONF,
		 "add OCaml interface (DEPRECATED)"),
  AP_INIT_TAKE1 ("CamlAddInterfacePath", cmd_add_interface_path,
		 NULL,
		 RSRC_CONF,
		 "add OCaml interface path (DEPRECATED)"),
  AP_INIT_TAKE1 ("CamlTranslateHandler", cmd_translate_handler,
		 NULL,
		 RSRC_CONF,
		 "set module as translate handler"),
  AP_INIT_TAKE1 ("CamlCheckUserIDHandler", cmd_check_user_id_handler,
		 NULL,
		 RSRC_CONF|ACCESS_CONF|OR_ALL,
		 "set module as check user ID handler"),
  AP_INIT_TAKE1 ("CamlAuthCheckerHandler", cmd_auth_checker_handler,
		 NULL,
		 RSRC_CONF|ACCESS_CONF|OR_ALL,
		 "set module as auth checker handler"),
  AP_INIT_TAKE1 ("CamlAccessCheckerHandler", cmd_access_checker_handler,
		 NULL,
		 RSRC_CONF|ACCESS_CONF|OR_ALL,
		 "set module as access checker handler"),
  AP_INIT_TAKE1 ("CamlTypeCheckerHandler", cmd_type_checker_handler,
		 NULL,
		 RSRC_CONF|ACCESS_CONF|OR_ALL,
		 "set module as type checker handler"),
  AP_INIT_TAKE1 ("CamlFixerUpperHandler", cmd_fixer_upper_handler,
		 NULL,
		 RSRC_CONF|ACCESS_CONF|OR_ALL,
		 "set module as fixer upper handler"),
  AP_INIT_TAKE1 ("CamlLoggerHandler", cmd_logger_handler,
		 NULL,
		 RSRC_CONF|ACCESS_CONF|OR_ALL,
		 "set module as logger handler"),
  AP_INIT_TAKE1 ("CamlHeaderParserHandler", cmd_header_parser_handler,
		 NULL,
		 RSRC_CONF|ACCESS_CONF|OR_ALL,
		 "set module as header parser handler"),
  AP_INIT_TAKE1 ("CamlPostReadRequestHandler", cmd_post_read_request_handler,
		 NULL,
		 RSRC_CONF|ACCESS_CONF|OR_ALL,
		 "set module as post read request handler"),
  AP_INIT_TAKE1 ("CamlHandler", cmd_handler,
		 NULL,
		 RSRC_CONF|ACCESS_CONF|OR_ALL,
		 "set module as output handler"),
  { NULL }
};

static void
register_hooks (apr_pool_t *p)
{
  ap_hook_post_config (post_config,
		       NULL, NULL, APR_HOOK_MIDDLE);
  ap_hook_translate_name (translate_handler,
			  NULL, NULL, APR_HOOK_MIDDLE);
  ap_hook_check_user_id (check_user_id,
			 NULL, NULL, APR_HOOK_MIDDLE);
  ap_hook_auth_checker (auth_checker,
			NULL, NULL, APR_HOOK_MIDDLE);
  ap_hook_access_checker (access_checker,
			NULL, NULL, APR_HOOK_MIDDLE);
  ap_hook_type_checker (type_checker,
			NULL, NULL, APR_HOOK_MIDDLE);
  ap_hook_fixups (fixer_upper,
		  NULL, NULL, APR_HOOK_MIDDLE);
  ap_hook_log_transaction (logger,
			   NULL, NULL, APR_HOOK_MIDDLE);
  ap_hook_header_parser (header_parser,
			 NULL, NULL, APR_HOOK_MIDDLE);
  ap_hook_post_read_request (post_read_request,
			     NULL, NULL, APR_HOOK_MIDDLE);
  ap_hook_handler (ocaml_bytecode_handler,
		   NULL, NULL, APR_HOOK_MIDDLE);
}

module caml_module = {
  STANDARD20_MODULE_STUFF,
  create_dir_config,
  merge_dir_config,
  create_server_config,
  merge_server_config,
  cmds,
  register_hooks,
};

#else

static command_rec cmds[] = {
  { "CamlLoad", cmd_load,
    NULL,
    RSRC_CONF, TAKE1,
    "load OCaml module" },
  { "CamlAddInterface", cmd_add_interface,
    NULL,
    RSRC_CONF, TAKE1,
    "add OCaml interface (DEPRECATED)" },
  { "CamlAddInterfacePath", cmd_add_interface_path,
    NULL,
    RSRC_CONF, TAKE1,
    "add OCaml interface path (DEPRECATED)" },
  { "CamlTranslateHandler", cmd_translate_handler,
    NULL,
    RSRC_CONF, TAKE1,
    "set module as translate handler" },
  { "CamlCheckUserIDHandler", cmd_check_user_id_handler,
    NULL,
    RSRC_CONF|ACCESS_CONF|OR_ALL, TAKE1,
    "set module as check user ID handler" },
  { "CamlAuthCheckerHandler", cmd_auth_checker_handler,
    NULL,
    RSRC_CONF|ACCESS_CONF|OR_ALL, TAKE1,
    "set module as auth checker handler" },
  { "CamlAccessCheckerHandler", cmd_access_checker_handler,
    NULL,
    RSRC_CONF|ACCESS_CONF|OR_ALL, TAKE1,
    "set module as access checker handler" },
  { "CamlTypeCheckerHandler", cmd_type_checker_handler,
    NULL,
    RSRC_CONF|ACCESS_CONF|OR_ALL, TAKE1,
    "set module as type checker handler" },
  { "CamlFixerUpperHandler", cmd_fixer_upper_handler,
    NULL,
    RSRC_CONF|ACCESS_CONF|OR_ALL, TAKE1,
    "set module as fixer upper handler" },
  { "CamlLoggerHandler", cmd_logger_handler,
    NULL,
    RSRC_CONF|ACCESS_CONF|OR_ALL, TAKE1,
    "set module as logger handler" },
  { "CamlHeaderParserHandler", cmd_header_parser_handler,
    NULL,
    RSRC_CONF|ACCESS_CONF|OR_ALL, TAKE1,
    "set module as header parser handler" },
  { "CamlPostReadRequestHandler", cmd_post_read_request_handler,
    NULL,
    RSRC_CONF|ACCESS_CONF|OR_ALL, TAKE1,
    "set module as post read request handler" },
  { "CamlHandler", cmd_handler,
    NULL,
    RSRC_CONF|ACCESS_CONF|OR_ALL, TAKE1,
    "set module as output handler" },
  { NULL }
};

static handler_rec handlers[] = {
  { "ocaml-bytecode", ocaml_bytecode_handler },
  { NULL }
};

module caml_module = {
  STANDARD_MODULE_STUFF,
  module_init,			/* initializer */
  create_dir_config,		/* create per-directory config */
  merge_dir_config,		/* merge per-directory config */
  create_server_config,		/* create per-server config */
  merge_server_config,		/* merge per-server config */
  cmds,				/* command table */
  handlers,			/* handlers */
  translate_handler,		/* translate handler */
  check_user_id,		/* check user id */
  auth_checker,			/* check authorization */
  access_checker,		/* check access */
  type_checker,			/* type mapping */
  fixer_upper,			/* pre-run fixups */
  logger,			/* logging */
  header_parser,		/* header parsing */
  NULL,				/* child initialization */
  NULL,				/* child exit */
  post_read_request,		/* post-read request */
};

#endif /* not APACHE2 */
