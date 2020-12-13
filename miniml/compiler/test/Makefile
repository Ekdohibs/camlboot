define ERRMSG
Fatal error:
This Makefile depends on an OCAMLRUN variable pointing to an
ocamlrun binary compatible with the bytecode produced by the
compiler.

Running 'make ocamlrun' at the root of this repository will
populate ../../_boot/byterun with a compatible 'ocamlrun',
which is used by default.

A standard way to invoke this testsuite is by using the
'test-compiler' target of the root makefile.
endef

OCAMLRUN?=../../../_boot/byterun/ocamlrun
ifeq "$(wildcard $(OCAMLRUN))" ""
$(error $(ERRMSG))
endif

COMPILE_CMD=GUILE_WARN_DEPRECATED=detailed guile ../compile.scm
COMPILE_DEPS=../compile.scm

TESTS=\
	empty \
	arith \
	functions \
	patterns \
	lists \
	labels \
	records \
	exceptions \
	let_open \
	infix_sugar \
	functors \
	exits \
	external_exceptions

.PHONY: all
all: $(addprefix test-,$(TESTS))

.PHONY: promote
promote: $(addprefix promote-,$(TESTS))

.PHONY: clean
clean:
	rm -f *.byte *.output *.info

test-%: %.byte %.output %.info
	diff -u --report-identical-files $*.output.reference $*.output
	diff -u --report-identical-files $*.info.reference $*.info

promote-%: %.byte %.output %.info
	cp $*.output $*.output.reference
	cp $*.info $*.info.reference

.PHONY: always-rerun
always-rerun:

%.byte: always-rerun %.ml $(COMPILE_DEPS) lib.ml
	$(COMPILE_CMD) lib.ml --open Lib $*.ml -o $*.byte

%.output: always-rerun %.byte
	$(OCAMLRUN) $*.byte > $*.output

%.info: always-rerun %.byte
	rm -f $*.info
	stat --printf="Bytecode size: %7s bytes\n" $*.byte >> $*.info
