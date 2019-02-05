#include <caml/sys.h>

#define CAMLparam6(a,b,c,d,e,f) CAMLparam5(a,b,c,d,e);CAMLxparam1(f)
#define CAMLparam7(a,b,c,d,e,f,g) CAMLparam6(a,b,c,d,e,f);CAMLxparam1(g)
#define CAMLparam8(a,b,c,d,e,f,g,h) CAMLparam7(a,b,c,d,e,f,g);CAMLxparam1(h)
#define CAMLparam9(a,b,c,d,e,f,g,h,i) CAMLparam8(a,b,c,d,e,f,g,h);CAMLxparam1(i)
#define CAMLparam10(a,b,c,d,e,f,g,h,i,j) CAMLparam9(a,b,c,d,e,f,g,h,i);CAMLxparam1(j)

value plus(value x, value y) { return Val_long(Long_val(x) + Long_val(y)); }
value minus(value x, value y) { return Val_long(Long_val(x) - Long_val(y)); }
value times(value x, value y) { return Val_long(Long_val(x) * Long_val(y)); }
value div_(value x, value y) { return Val_long(Long_val(x) / Long_val(y)); }
value uminus(value x) { return Val_long(-Long_val(x)); }
value mod_(value x, value y) { return Val_long(Long_val(x) % Long_val(y)); }
value land_(value x, value y) { return Val_long(Long_val(x) & Long_val(y)); }
value lor_(value x, value y) { return Val_long(Long_val(x) | Long_val(y)); }
value lxor_(value x, value y) { return Val_long(Long_val(x) ^ Long_val(y)); }
value lsl_(value x, value y) { return Val_long(Long_val(x) << Long_val(y)); }
value asr_(value x, value y) { return Val_long(((long long int)Long_val(x)) >> Long_val(y)); }
value lsr_(value x, value y) { return Val_long(((unsigned long long int)Long_val(x)) >> Long_val(y)); }
value caml_eq(value x, value y) { return Long_val(x == y); }
value caml_noteq(value x, value y) { return Long_val(x != y); }

value caml_assert(value x) { assert (Long_val(x)); return Val_unit; }
value fst(value x) { return Field(x, 0); }
value snd(value x) { return Field(x, 1); }

value caml_obj_size(value x) { return Val_long(Wosize_val(x)); }
value caml_obj_field(value x, value i) { return Field(x, Long_val(i)); }
value caml_obj_set_field(value x, value i, value v) {
  CAMLparam3(x, i, v);
  Store_field(x, Long_val(i), v);
  CAMLdrop; return Val_unit;
}
value caml_obj_is_int(value x) { return Val_long(Is_long(x)); }