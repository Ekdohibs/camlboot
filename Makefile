BOOT=_boot
OCAMLSRC=ocaml-src
CONFIG=$(OCAMLSRC)/config/Makefile
OCAMLRUN=$(OCAMLSRC)/byterun/ocamlrun
GENERATED=$(OCAMLSRC)/bytecomp/opcodes.ml

$(OCAMLRUN): $(CONFIG)
	make -C $(OCAMLSRC)/byterun all
	make -C $(OCAMLSRC)/asmrun all

.PHONY: configure-ocaml
configure-ocaml:
	rm -f $(OCAMLSRC)/boot/ocamlc $(OCAMLSRC)/boot/ocamllex
	cd $(OCAMLSRC) && bash configure
	make -C $(OCAMLSRC) ocamlyacc && cp $(OCAMLSRC)/yacc/ocamlyacc $(OCAMLSRC)/boot
	make -C $(OCAMLSRC)/lex parser.ml

.PHONY: ocaml-generated-files
ocaml-generated-files: $(OCAMLRUN) lex make_opcodes cvt_emit
	make -C $(OCAMLSRC)/stdlib sys.ml
	make -C $(OCAMLSRC) utils/config.ml
	make -C $(OCAMLSRC) parsing/parser.ml
	miniml/interp/lex.sh $(OCAMLSRC)/parsing/lexer.mll -o $(OCAMLSRC)/parsing/lexer.ml
	make -C $(OCAMLSRC) bytecomp/runtimedef.ml
	miniml/interp/make_opcodes.sh -opcodes < $(OCAMLSRC)/byterun/caml/instruct.h > $(OCAMLSRC)/bytecomp/opcodes.ml
	make -C $(OCAMLSRC) asmcomp/arch.ml asmcomp/proc.ml asmcomp/selection.ml asmcomp/CSE.ml asmcomp/reload.ml asmcomp/scheduling.ml
	miniml/interp/cvt_emit.sh < $(OCAMLSRC)/asmcomp/amd64/emit.mlp > $(OCAMLSRC)/asmcomp/emit.ml

.PHONY: lex
lex: $(OCAMLRUN)
	make -C miniml/interp lex.byte

.PHONY: make_opcodes
make_opcodes: $(OCAMLRUN)
	make -C miniml/interp make_opcodes.byte

.PHONY: cvt_emit
cvt_emit: $(OCAMLRUN)
	make -C miniml/interp cvt_emit.byte


.PHONY: clean-ocaml-config
clean-ocaml-config:
	cd $(OCAMLSRC) && make distclean

# this dependency is fairly coarse-grained, so feel free to
# use clean-ocaml-config if make a small change to $(OCAMLSRC)
# that you believe does require re-configuring.
$(CONFIG): $(OCAMLSRC)/VERSION
	make configure-ocaml

$(GENERATED): $(OCAMLRUN) lex make_opcodes
	make ocaml-generated-files

$(BOOT)/driver: $(OCAMLSRC)/driver $(OCAMLSRC)/otherlibs/dynlink $(CONFIG) $(GENERATED)
	mkdir -p $(BOOT)
	rm -rf $@
	cp -r $< $@
	cp $(OCAMLSRC)/otherlibs/dynlink/dynlink.mli $@/compdynlink.mli
	grep -v 'REMOVE_ME for ../../debugger/dynlink.ml' \
	     $(OCAMLSRC)/otherlibs/dynlink/dynlink.ml > $@/compdynlink.mlbyte

$(BOOT)/byterun: $(OCAMLSRC)/byterun $(CONFIG) $(GENERATED)
	mkdir -p $(BOOT)
	rm -rf $@
	cp -r $< $@

$(BOOT)/bytecomp: $(OCAMLSRC)/bytecomp $(CONFIG) $(GENERATED)
	mkdir -p $(BOOT)
	rm -rf $@
	cp -r $< $@

$(BOOT)/typing: $(OCAMLSRC)/typing $(CONFIG) $(GENERATED)
	mkdir -p $(BOOT)
	rm -rf $@
	cp -r $< $@

$(BOOT)/parsing: $(OCAMLSRC)/parsing $(CONFIG) $(GENERATED) patches/parsetree.patch lex
	mkdir -p $(BOOT)
	rm -rf $@
	cp -r $< $@
	#patch $(BOOT)/parsing/parsetree.mli patches/parsetree.patch

$(BOOT)/utils: $(OCAMLSRC)/utils $(CONFIG) $(GENERATED) patches/disable-profiling.patch
	mkdir -p $(BOOT)
	rm -rf $@
	cp -r $< $@
	cp $(BOOT)/utils/profile.ml $(BOOT)/utils/profile.ml.noprof
	patch $(BOOT)/utils/profile.ml.noprof patches/disable-profiling.patch

$(BOOT)/stdlib: $(OCAMLSRC)/stdlib $(CONFIG) $(GENERATED) patches/compflags.patch
	mkdir -p $(BOOT)
	rm -rf $@
	cp -r $< $@
	patch $(BOOT)/stdlib/Compflags patches/compflags.patch
	awk -f $(BOOT)/stdlib/expand_module_aliases.awk < $(BOOT)/stdlib/stdlib.mli > $(BOOT)/stdlib/stdlib.pp.mli
	awk -f $(BOOT)/stdlib/expand_module_aliases.awk < $(BOOT)/stdlib/stdlib.ml > $(BOOT)/stdlib/stdlib.pp.ml
	cp $(OCAMLSRC)/asmrun/libasmrun.a $(BOOT)/stdlib/

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

$(BOOT)/ocamlc: $(COPY_TARGETS)
	make -C $(OCAMLSRC)/yacc all
	make -C miniml/interp interp.byte
	echo "stdlib compilation:" > timings
	cd $(BOOT)/stdlib && /usr/bin/env time -a -o ../../timings ../../compile_stdlib.sh
	mkdir -p $(BOOT)/compilerlibs
	echo "ocamlc compilation:" >> timings 
	cd $(BOOT) && /usr/bin/env time -a -o ../timings ../compile_ocamlc.sh

.PHONY: test-compiler
test-compiler: $(OCAMLRUN)
	make -C miniml/compiler/test all OCAMLRUN=../../../$(OCAMLRUN)

.PHONY: test-compiler-promote
test-compiler-promote: $(OCAMLRUN)
	make -C miniml/compiler/test promote OCAMLRUN=../../../$(OCAMLRUN)
