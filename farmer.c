/* See https://ocaml.org/manual/intfc.html */

#include <unistd.h>
#include <errno.h>
#include <string.h>
#include <caml/mlvalues.h>
#include <caml/alloc.h>

CAMLprim value caml_symlink_wrapper(value dst, value src) {
  int res = symlink(String_val(dst), String_val(src));
  return Val_int(res);
}

CAMLprim value caml_strerror_wrapper(value _unit) {
  return caml_copy_string(strerror(errno));
}
