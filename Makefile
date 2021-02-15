BOOT=_boot
OCAMLSRC=ocaml-src
CONFIG=$(OCAMLSRC)/config/Makefile
OCAMLRUN=_boot/byterun/ocamlrun

.PHONY: configure-ocaml
configure-ocaml:
	rm $(OCAMLSRC)/boot/ocamlc $(OCAMLSRC)/boot/ocamllex
	cd $(OCAMLSRC) && bash configure
	make -C $(OCAMLSRC) ocamlyacc && cp $(OCAMLSRC)/yacc/ocamlyacc $(OCAMLSRC)/boot 
	make -C $(OCAMLSRC)/stdlib sys.ml
	make -C $(OCAMLSRC) utils/config.ml
	make -C $(OCAMLSRC) parsing/parser.ml
	#make -C $(OCAMLSRC) CAMLLEX=ocamllex CAMLRUN=ocamlrun parsing/lexer.ml
	make -C $(OCAMLSRC) bytecomp/runtimedef.ml
	#make -C $(OCAMLSRC) CAMLLEX=ocamllex CAMLRUN=ocamlrun CAMLC=ocamlc bytecomp/opcodes.ml

.PHONY: lex
lex:
	make -C miniml/interp lex.byte

.PHONY: make_opcodes
make_opcodes:
	make -C miniml/interp make_opcodes.byte

.PHONY: clean-ocaml-config
clean-ocaml-config:
	cd $(OCAMLSRC) && make distclean

# this dependency is fairly coarse-grained, so feel free to
# use clean-ocaml-config if make a small change to $(OCAMLSRC)
# that you believe does require re-configuring.
$(CONFIG): $(OCAMLSRC)/VERSION
	make configure-ocaml

$(BOOT)/driver: $(OCAMLSRC)/driver $(OCAMLSRC)/otherlibs/dynlink $(CONFIG)
	mkdir -p $(BOOT)
	rm -rf $@
	cp -r $< $@
	cp $(OCAMLSRC)/otherlibs/dynlink/dynlink.mli $@/compdynlink.mli
	grep -v 'REMOVE_ME for ../../debugger/dynlink.ml' \
	     $(OCAMLSRC)/otherlibs/dynlink/dynlink.ml > $@/compdynlink.mlbyte

$(BOOT)/byterun: $(OCAMLSRC)/byterun $(CONFIG)
	make -C $(OCAMLSRC)/byterun all
	mkdir -p $(BOOT)
	rm -rf $@
	cp -r $< $@

$(BOOT)/bytecomp: $(OCAMLSRC)/bytecomp $(CONFIG)
	mkdir -p $(BOOT)
	rm -rf $@
	cp -r $< $@
	miniml/interp/make_opcodes.sh -opcodes < $(OCAMLSRC)/byterun/caml/instruct.h > $(BOOT)/bytecomp/opcodes.ml

$(BOOT)/typing: $(OCAMLSRC)/typing $(CONFIG)
	mkdir -p $(BOOT)
	rm -rf $@
	cp -r $< $@

$(BOOT)/parsing: $(OCAMLSRC)/parsing $(CONFIG) patches/parsetree.patch lex
	mkdir -p $(BOOT)
	rm -rf $@
	cp -r $< $@
	patch $(BOOT)/parsing/parsetree.mli patches/parsetree.patch
	miniml/interp/lex.sh $(BOOT)/parsing/lexer.mll -o $(BOOT)/parsing/lexer.ml

$(BOOT)/utils: $(OCAMLSRC)/utils $(CONFIG) patches/disable-profiling.patch
	mkdir -p $(BOOT)
	rm -rf $@
	cp -r $< $@
	cp $(BOOT)/utils/profile.ml $(BOOT)/utils/profile.ml.noprof
	patch $(BOOT)/utils/profile.ml.noprof patches/disable-profiling.patch

$(BOOT)/stdlib: $(OCAMLSRC)/stdlib $(CONFIG) patches/compflags.patch
	mkdir -p $(BOOT)
	rm -rf $@
	cp -r $< $@
	patch $(BOOT)/stdlib/Compflags patches/compflags.patch
	awk -f $(BOOT)/stdlib/expand_module_aliases.awk < $(BOOT)/stdlib/stdlib.mli > $(BOOT)/stdlib/stdlib.pp.mli
	awk -f $(BOOT)/stdlib/expand_module_aliases.awk < $(BOOT)/stdlib/stdlib.ml > $(BOOT)/stdlib/stdlib.pp.ml

COPY_TARGETS=\
	$(BOOT)/bytecomp \
	$(BOOT)/byterun \
	$(BOOT)/driver \
	$(BOOT)/parsing \
	$(BOOT)/stdlib \
	$(BOOT)/typing \
	$(BOOT)/utils

.PHONY: copy
copy: $(COPY_TARGETS)

.PHONY: ocamlrun
ocamlrun: $(OCAMLRUN)

$(OCAMLRUN): $(BOOT)/byterun

$(BOOT)/ocamlc: $(COPY_TARGETS)
	make -C $(OCAMLSRC)/yacc all
	make -C miniml/interp interp
	cd $(BOOT)/stdlib && ../../compile_stdlib.sh
	mkdir -p $(BOOT)/compilerlibs
	cd $(BOOT) && ../compile_ocamlc.sh

.PHONY: test-compiler
test-compiler: $(OCAMLRUN)
	make -C miniml/compiler/test all OCAMLRUN=../../../$(OCAMLRUN)

.PHONY: test-compiler-promote
test-compiler-promote: $(OCAMLRUN)
	make -C miniml/compiler/test promote OCAMLRUN=../../../$(OCAMLRUN)
