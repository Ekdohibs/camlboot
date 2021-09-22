BOOT=_boot
OCAMLSRC=ocaml-src
CONFIG=$(OCAMLSRC)/config/Makefile
OCAMLRUN=$(OCAMLSRC)/byterun/ocamlrun
GENERATED=$(OCAMLSRC)/bytecomp/opcodes.ml

$(OCAMLRUN): $(CONFIG)
	touch $(OCAMLSRC)/byterun/.depend && $(MAKE) -C $(OCAMLSRC)/byterun depend
	$(MAKE) -C $(OCAMLSRC)/byterun all
	touch $(OCAMLSRC)/asmrun/.depend && $(MAKE) -C $(OCAMLSRC)/asmrun depend
	$(MAKE) -C $(OCAMLSRC)/asmrun all

.PHONY: configure-ocaml
configure-ocaml:
	rm -f $(OCAMLSRC)/boot/ocamlc $(OCAMLSRC)/boot/ocamllex
	find $(OCAMLSRC) -iname .depend | xargs rm -f
	touch $(OCAMLSRC)/.depend $(OCAMLSRC)/stdlib/.depend $(OCAMLSRC)/lex/.depend
	cd $(OCAMLSRC) && bash configure
	$(MAKE) -C $(OCAMLSRC) ocamlyacc && cp $(OCAMLSRC)/yacc/ocamlyacc $(OCAMLSRC)/boot
	$(MAKE) -C $(OCAMLSRC)/lex parser.ml

# Here, including $(CONFIG) would provide $(ARCH), but it leads to a recursive
# dependency because its rule has a dependency that reloads this Makefile.
.PHONY: ocaml-generated-files
ocaml-generated-files: $(OCAMLRUN) lex make_opcodes cvt_emit
	$(MAKE) -C $(OCAMLSRC)/stdlib sys.ml
	$(MAKE) -C $(OCAMLSRC) utils/config.ml
	$(MAKE) -C $(OCAMLSRC) parsing/parser.ml
	cd $(OCAMLSRC); ../miniml/interp/lex.sh parsing/lexer.mll
	$(MAKE) -C $(OCAMLSRC) bytecomp/runtimedef.ml
	miniml/interp/make_opcodes.sh -opcodes < $(OCAMLSRC)/byterun/caml/instruct.h > $(OCAMLSRC)/bytecomp/opcodes.ml
	$(MAKE) -C $(OCAMLSRC) asmcomp/arch.ml asmcomp/proc.ml asmcomp/selection.ml asmcomp/CSE.ml asmcomp/reload.ml asmcomp/scheduling.ml
	miniml/interp/cvt_emit.sh < $(OCAMLSRC)/asmcomp/$(shell cat $(CONFIG) | grep '^ARCH=' | cut -f2 -d=)/emit.mlp > $(OCAMLSRC)/asmcomp/emit.ml

.PHONY: lex
lex: $(OCAMLRUN)
	touch miniml/interp/.depend
	$(MAKE) -C miniml/interp lex.byte

.PHONY: make_opcodes
make_opcodes: $(OCAMLRUN) lex
	$(MAKE) -C miniml/interp make_opcodes.byte

.PHONY: cvt_emit
cvt_emit: $(OCAMLRUN) lex
	$(MAKE) -C miniml/interp cvt_emit.byte

.PHONY: makedepend
makedepend: $(OCAMLRUN) lex
	$(MAKE) -C miniml/interp makedepend.byte

.PHONY: clean-ocaml-config
clean-ocaml-config:
	cd $(OCAMLSRC) && make distclean

# this dependency is fairly coarse-grained, so feel free to
# use clean-ocaml-config if make a small change to $(OCAMLSRC)
# that you believe does require re-configuring.
$(CONFIG): $(OCAMLSRC)/VERSION
	$(MAKE) configure-ocaml

$(GENERATED): $(OCAMLRUN) lex make_opcodes
	$(MAKE) ocaml-generated-files

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
	cp Makefile.stdlib $(BOOT)/stdlib/Makefile

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
	cp Makefile.ocamlc $(BOOT)/Makefile

.PHONY: ocamlrun
ocamlrun: $(OCAMLRUN)

$(BOOT)/ocamlc: copy makedepend
	$(MAKE) -C $(OCAMLSRC)/yacc all
	$(MAKE) -C miniml/interp depend
	./timed.sh $(MAKE) $(MAKEFLAGS) -C miniml/interp interpopt.opt
	touch _boot/stdlib/.depend && $(MAKE) -C _boot/stdlib depend
	touch _boot/.depend && $(MAKE) -C _boot depend
	./timed.sh $(MAKE) $(MAKEFLAGS) -C _boot/stdlib all
	# cd $(BOOT)/stdlib && ../../timed.sh ../../compile_stdlib.sh
	mkdir -p $(BOOT)/compilerlibs
	./timed.sh $(MAKE) $(MAKEFLAGS) -C _boot ocamlc
	# cd $(BOOT) && ../timed.sh ../compile_ocamlc.sh

# Remove dependency on $(BOOT)/ocamlc, because it seems to cause ocamlc to be rebuilt even if it was just built
fullboot:
	cp $(BOOT)/ocamlc $(OCAMLSRC)/boot/
	cp miniml/interp/lex.byte $(OCAMLSRC)/boot/ocamllex
	cp $(OCAMLSRC)/byterun/ocamlrun $(OCAMLSRC)/boot/ocamlrun$(EXE)
	touch $(OCAMLSRC)/stdlib/.depend && ./timed.sh $(MAKE) $(MAKEFLAGS) -C $(OCAMLSRC)/stdlib CAMLDEP="../boot/ocamlc -depend" depend
	./timed.sh $(MAKE) $(MAKEFLAGS) -C $(OCAMLSRC)/stdlib COMPILER="" CAMLC="../boot/ocamlc -use-prims ../byterun/primitives" all
	cd $(OCAMLSRC)/stdlib; cp stdlib.cma std_exit.cmo *.cmi camlheader ../boot
	cd $(OCAMLSRC)/boot; ln -sf ../byterun/libcamlrun.a .
	touch $(OCAMLSRC)/tools/.depend &&  ./timed.sh $(MAKE) $(MAKEFLAGS) -C $(OCAMLSRC)/tools CAMLC="../boot/ocamlc -nostdlib -I ../boot -use-prims ../byterun/primitives -I .." make_opcodes cvt_emit
	touch $(OCAMLSRC)/lex/.depend && ./timed.sh $(MAKE) $(MAKEFLAGS) -C $(OCAMLSRC)/lex CAMLDEP="../boot/ocamlc -depend" depend
	./timed.sh $(MAKE) $(MAKEFLAGS) -C $(OCAMLSRC) CAMLDEP="boot/ocamlc -depend" depend
	./timed.sh $(MAKE) $(MAKEFLAGS) -C $(OCAMLSRC) CAMLC="boot/ocamlc -nostdlib -I boot -use-prims byterun/primitives" ocamlc
	./timed.sh $(MAKE) $(MAKEFLAGS) -C $(OCAMLSRC)/lex CAMLC="../boot/ocamlc -strict-sequence -nostdlib -I ../boot -use-prims ../byterun/primitives" all

.PHONY: test-compiler
test-compiler: $(OCAMLRUN)
	$(MAKE) -C miniml/compiler/test all OCAMLRUN=../../../$(OCAMLRUN)

.PHONY: test-compiler-promote
test-compiler-promote: $(OCAMLRUN)
	$(MAKE) -C miniml/compiler/test promote OCAMLRUN=../../../$(OCAMLRUN)
