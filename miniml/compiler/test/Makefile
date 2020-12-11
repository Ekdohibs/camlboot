OCAMLRUN_CMD=$(or $(OCAMLRUN),$(error $(ERRMSG)))
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


COMPILE_CMD=GUILE_WARN_DEPRECATED=detailed guile ../compile.scm
COMPILE_DEPS=../compile.scm

.PHONY: all
all:
	$(MAKE) clean
	$(MAKE) test.output
	diff -u --report-identical-files test.{reference,output}

test.output: $(COMPILE_DEPS) test.ml
	$(COMPILE_CMD) test.ml -o test.byte
	$(OCAMLRUN_CMD) test.byte > test.output

promote: test.output
	cp test.output test.reference

clean:
	rm -f test.byte test.output
