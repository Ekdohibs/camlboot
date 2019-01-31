#include <caml/sys.h>

#define CAMLparam6(a,b,c,d,e,f) CAMLparam5(a,b,c,d,e);CAMLxparam1(f)
#define CAMLparam7(a,b,c,d,e,f,g) CAMLparam6(a,b,c,d,e,f);CAMLxparam1(g)
#define CAMLparam8(a,b,c,d,e,f,g,h) CAMLparam7(a,b,c,d,e,f,g);CAMLxparam1(h)
#define CAMLparam9(a,b,c,d,e,f,g,h,i) CAMLparam8(a,b,c,d,e,f,g,h);CAMLxparam1(i)
#define CAMLparam10(a,b,c,d,e,f,g,h,i,j) CAMLparam9(a,b,c,d,e,f,g,h,i);CAMLxparam1(j)

value plus(value x, value y) { return Val_long(Long_val(x) + Long_val(y)); }
value minus(value x, value y) { return Val_long(Long_val(x) - Long_val(y)); }
value times(value x, value y) { return Val_long(Long_val(x) * Long_val(y)); }
value caml_assert(value x) { assert (Long_val(x)); return Val_long(0); }
value fst(value x) { return Field(x, 0); }
value snd(value x) { return Field(x, 1); }
