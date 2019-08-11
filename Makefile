OBJS=conf.cmo data.cmo envir.cmo \
	runtime_lib.cmo runtime_base.cmo \
	eval.cmo \
	runtime_stdlib.cmo runtime_compiler.cmo \
	primitives.cmo \
	interp.cmo
SRCS=$(OBJS:.cmo=.ml)
FLAGS=-g -package unix -package compiler-libs.common -linkpkg
OCAML=ocamlfind ocamlc
OCAMLOPT=ocamlfind ocamlopt

.PHONY: all clean format
all: interp interpopt

clean:
	for f in $(wildcard *.cm*) $(wildcard *.o); do rm $$f; done

format:
	ocamlformat --inplace $(SRCS)


.SUFFIXES: .mli .ml .cmi .cmo .cmx

.ml.cmx:
	$(OCAMLOPT) $(FLAGS) -c $<

.ml.cmo:
	$(OCAML) $(FLAGS) -c $<

.depend: $(SRCS)
	ocamldep $(SRCS) > .depend

include .depend

interp: $(OBJS)
	echo $(OCAML) $(FLAGS) -linkpkg -o $@ $+
	$(OCAML) $(FLAGS) -linkpkg -o $@ $+

interpopt: $(OBJS:.cmo=.cmx)
	$(OCAMLOPT) $(FLAGS) -linkpkg -o $@ $+

BOOT=_boot
OCAMLSRC=ocaml-src

configure-ocaml: $(OCAMLSRC)
	cd $(OCAMLSRC) && bash configure
	make -C $(OCAMLSRC) ocamlyacc && cp $(OCAMLSRC)/yacc/ocamlyacc $(OCAMLSRC)/boot 
	make -C $(OCAMLSRC)/stdlib sys.ml
	make -C $(OCAMLSRC) utils/config.ml
	make -C $(OCAMLSRC) parsing/parser.ml
	make -C $(OCAMLSRC) CAMLLEX=ocamllex CAMLRUN=ocamlrun parsing/lexer.ml
	make -C $(OCAMLSRC) bytecomp/runtimedef.ml
	make -C $(OCAMLSRC) CAMLLEX=ocamllex CAMLRUN=ocamlrun CAMLC=ocamlc bytecomp/opcodes.ml

$(BOOT)/driver: $(OCAMLSRC)/driver $(OCAMLSRC)/otherlibs/dynlink configure-ocaml
	mkdir -p $(BOOT)
	rm -rf $@
	cp -r $< $@
	cp $(OCAMLSRC)/otherlibs/dynlink/dynlink.mli $@/compdynlink.mli
	grep -v 'REMOVE_ME for ../../debugger/dynlink.ml' \
	     $(OCAMLSRC)/otherlibs/dynlink/dynlink.ml > $@/compdynlink.mlbyte

$(BOOT)/byterun: $(OCAMLSRC)/byterun configure-ocaml
	make -C $(OCAMLSRC)/byterun all
	mkdir -p $(BOOT)
	rm -rf $@
	cp -r $< $@

$(BOOT)/bytecomp: $(OCAMLSRC)/bytecomp configure-ocaml
	mkdir -p $(BOOT)
	rm -rf $@
	cp -r $< $@

$(BOOT)/typing: $(OCAMLSRC)/typing configure-ocaml
	mkdir -p $(BOOT)
	rm -rf $@
	cp -r $< $@

$(BOOT)/parsing: $(OCAMLSRC)/parsing configure-ocaml patches/parsetree.patch
	mkdir -p $(BOOT)
	rm -rf $@
	cp -r $< $@
	patch $(BOOT)/parsing/parsetree.mli patches/parsetree.patch

$(BOOT)/utils: $(OCAMLSRC)/utils configure-ocaml patches/disable-profiling.patch
	mkdir -p $(BOOT)
	rm -rf $@
	cp -r $< $@
	cp $(BOOT)/utils/profile.ml $(BOOT)/utils/profile.ml.noprof
	patch $(BOOT)/utils/profile.ml.noprof patches/disable-profiling.patch

$(BOOT)/stdlib: $(OCAMLSRC)/stdlib configure-ocaml patches/compflags.patch
	mkdir -p $(BOOT)
	rm -rf $@
	cp -r $< $@
	patch $(BOOT)/stdlib/Compflags patches/compflags.patch
	awk -f $(BOOT)/stdlib/expand_module_aliases.awk < $(BOOT)/stdlib/stdlib.mli > $(BOOT)/stdlib/stdlib.pp.mli
	awk -f $(BOOT)/stdlib/expand_module_aliases.awk < $(BOOT)/stdlib/stdlib.ml > $(BOOT)/stdlib/stdlib.pp.ml

copy: $(BOOT)/driver $(BOOT)/bytecomp $(BOOT)/byterun $(BOOT)/typing $(BOOT)/parsing $(BOOT)/utils $(BOOT)/stdlib

$(BOOT)/ocamlc: copy
	make -C $(OCAMLSRC)/yacc all
	make -C miniml/compiler miniml
	make -C miniml/interp interp
	cd $(BOOT)/stdlib && ../../compile_stdlib.sh
	mkdir -p $(BOOT)/compilerlibs
	cd $(BOOT) && ../compile_ocamlc.sh

.PHONY: run

run: interpopt
# we defined a symbolic link ./ocaml-src to point to the compiler sources,
# at a version copmatible with the OCAMLINTERP_STDLIB_PATH version.
	env \
	  OCAMLRUNPARAM=b \
	  OCAMLINTERP_DEBUG=true \
	  OCAMLINTERP_STDLIB_PATH=$(shell ocamlc -where) \
	  OCAMLINTERP_SRC_PATH=./ocaml-src \
	  ./interpopt
