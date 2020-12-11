define ERRMSG
Fatal error:
This Makefile depends on an OCAMLRUN variable pointing to an
ocamlrun binary compatible with the bytecode produced by the
compiler.

A standard way to invoke this testsuite is by using the
'test-compiler' target of the root makefile, which defines OCAMLRUN
automatically, using the ocamlrun of ocaml-src in _boot/.

If you have already run 'make ocamlrun' at the root, you may use the following:

make OCAMLRUN=../../../_boot/byterun/ocamlrun


endef

OCAMLRUN_CMD=$(or $(OCAMLRUN),$(error $(ERRMSG)))
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
	external_exceptions

.PHONY: all
all: $(addprefix test-,$(TESTS))

.PHONY: promote
promote: $(addprefix promote-,$(TESTS))

.PHONY: clean
clean:
	rm -f *.byte *.output *.info

test-%: %.byte %.output
	diff -u --report-identical-files $*.output.reference $*.output

promote-%: %.byte %.output
	cp $*.output $*.output.reference

.PHONY: always-rerun
always-rerun:

%.byte: always-rerun %.ml $(COMPILE_DEPS) lib.ml
	$(COMPILE_CMD) lib.ml --open Lib $*.ml -o $*.byte

%.output: always-rerun %.byte
	$(OCAMLRUN_CMD) $*.byte > $*.output
