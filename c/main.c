#include <stdio.h>
#include <caml/callback.h>
#include <caml/memory.h>
#include <assert.h>

extern value parse(char * filename);

int main(int argc, char ** argv)
{
  CAMLparam0();
  CAMLlocal1 (program);
  caml_startup(argv);
  program = parse("test.ml");
  assert (Is_block(program));
  printf("Tag = %d, Size = %ld\n", Tag_val(program), Wosize_val(program));
  return 0;
}