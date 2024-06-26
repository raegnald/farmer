/* See https://ocaml.org/manual/intfc.html */

#include <unistd.h>
#include <errno.h>
#include <string.h>
#include <fnmatch.h>
#include <caml/mlvalues.h>
#include <caml/alloc.h>

CAMLprim value caml_fnmatch_wrapper(value pattern, value str) {
  int found = fnmatch(String_val(pattern), String_val(str), FNM_PATHNAME);
  return Val_bool(found == 0);
}

CAMLprim value caml_symlink_wrapper(value dst, value src) {
  int res = symlink(String_val(dst), String_val(src));
  return Val_int(res);
}

CAMLprim value caml_strerror_wrapper(value _unit) {
  return caml_copy_string(strerror(errno));
}
