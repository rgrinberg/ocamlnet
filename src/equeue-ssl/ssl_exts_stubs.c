/* $Id$ */

#include <caml/alloc.h>
#include <caml/callback.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/misc.h>
#include <caml/mlvalues.h>
#include <caml/signals.h>
#include <openssl/ssl.h>
#include <openssl/pem.h>
#include <openssl/err.h>
#include <openssl/bio.h>
#include <unistd.h>

#define SSL_val(v) (*((SSL**)Data_custom_val(v)))


CAMLprim value ocaml_ssl_single_shutdown(value socket)
{
  CAMLparam1(socket);
  int ret;

  SSL *ssl = SSL_val(socket);
  caml_enter_blocking_section();
  ret = SSL_shutdown(ssl);
  if (ret == -1) {
      raise_with_arg(*caml_named_value("ssl_exn_shutdown_error"), 
		     Val_int(SSL_get_error(ssl, ret)));
  };
  caml_leave_blocking_section();

  CAMLreturn(Val_unit);
}


CAMLprim value ocaml_ssl_get_shutdown(value socket)
{
  CAMLparam1(socket);
  CAMLlocal3(rcvd,sent,ret);
  int r;
  
  SSL *ssl = SSL_val(socket);
  caml_enter_blocking_section();
  r = SSL_get_shutdown(ssl);
  caml_leave_blocking_section();
  rcvd = Val_bool(r & SSL_RECEIVED_SHUTDOWN);
  sent = Val_bool(r & SSL_SENT_SHUTDOWN);
  ret = alloc_tuple(2);
  Store_field(ret, 0, rcvd);
  Store_field(ret, 1, sent);

  CAMLreturn(ret);
}


CAMLprim value ocaml_ssl_get_rbio_eof(value socket) 
{
    CAMLparam1(socket);
    CAMLlocal1(ret);
    BIO *b;
    int eof;

    SSL *ssl = SSL_val(socket);
    caml_enter_blocking_section();
    b = SSL_get_rbio(ssl);
    caml_leave_blocking_section();
    if (b == NULL) 
	failwith("Ssl.get_rbio_eof: No rbio found");
    eof = BIO_eof(b);
    ret = Val_bool(eof);

    CAMLreturn(ret);
}


CAMLprim value ocaml_ssl_get_mode(value socket)
{
    CAMLparam1(socket);
    CAMLlocal1(ret);
    long m;
    SSL *ssl = SSL_val(socket);
    caml_enter_blocking_section();
    m = SSL_get_mode(ssl);
    caml_leave_blocking_section();
    ret = alloc_tuple(3);
    Store_field(ret, 0, Val_bool(m & SSL_MODE_ENABLE_PARTIAL_WRITE));
    Store_field(ret, 1, Val_bool(m & SSL_MODE_ACCEPT_MOVING_WRITE_BUFFER));
    Store_field(ret, 2, Val_bool(m & SSL_MODE_AUTO_RETRY));
    CAMLreturn(ret);
}

CAMLprim value ocaml_ssl_set_mode(value socket, value mode)
{
    CAMLparam2(socket,mode);
    long m;
    SSL *ssl = SSL_val(socket);
    m = 0;
    if (Bool_val(Field(mode, 0))) m |= SSL_MODE_ENABLE_PARTIAL_WRITE;
    if (Bool_val(Field(mode, 1))) m |= SSL_MODE_ACCEPT_MOVING_WRITE_BUFFER;
    if (Bool_val(Field(mode, 2))) m |= SSL_MODE_AUTO_RETRY;
    caml_enter_blocking_section();
    SSL_set_mode(ssl, m);
    caml_leave_blocking_section();
    CAMLreturn(Val_unit);
}



