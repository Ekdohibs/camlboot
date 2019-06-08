
OBJS=interp.cmo
FLAGS=-g -package compiler-libs.common -linkpkg
OCAML=ocamlfind ocamlc
OCAMLOPT=ocamlfind ocamlopt

.SUFFIXES: .mli .ml .cmi .cmo .cmx

.ml.cmx:
	$(OCAMLOPT) $(FLAGS) -c $<

.ml.cmo:
	$(OCAML) $(FLAGS) -c $<

interp: $(OBJS)
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

$(BOOT)/driver: $(OCAMLSRC)/driver configure-ocaml
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

copy: $(BOOT)/driver $(BOOT)/bytecomp $(BOOT)/typing $(BOOT)/parsing $(BOOT)/utils $(BOOT)/stdlib

$(BOOT)/ocamlc: copy
	cd $(BOOT)/stdlib && ../../compile_stdlib.sh
	cd $(BOOT) && ../compile_ocamlc.sh
