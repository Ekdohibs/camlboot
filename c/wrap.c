#include <stdio.h>
#include <string.h>
#include <caml/mlvalues.h>
#include <caml/callback.h>
#include <caml/memory.h>
#include <caml/alloc.h>

value parse(char * filename)
{
  CAMLparam0();
  CAMLlocal1 (fname);
  fname = caml_copy_string(filename);
  static value * parse_closure = NULL;
  if (parse_closure == NULL)
    parse_closure = caml_named_value("parse");
  CAMLreturn (caml_callback(*parse_closure, fname));
}
