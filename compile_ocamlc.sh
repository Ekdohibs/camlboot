#!/usr/bin/env bash

ulimit -s 2000000

export EXT=cmx
export AEXT=cmxa
#export COMPILER="../miniml/interp/interp -g -nostdlib -I stdlib -use-prims byterun/primitives"
export COMPILER="../miniml/interp/interp.opt -g -nostdlib -I stdlib"
export COMPFLAGS="-strict-sequence -principal -absname -w +a-4-9-41-42-44-45-48 -warn-error A -bin-annot -safe-string -strict-formats -I utils -I parsing -I typing -I bytecomp -I middle_end -I middle_end/base_types -I asmcomp -I asmcomp/debug -I driver -I toplevel"

compile () {
	echo $COMPILER $COMPFLAGS -c $@
	$COMPILER $COMPFLAGS -c $@
}

compile utils/config.mli
compile utils/config.ml
compile utils/misc.mli
compile utils/misc.ml
compile utils/identifiable.mli
compile utils/identifiable.ml
compile utils/numbers.mli
compile utils/numbers.ml
compile utils/arg_helper.mli
compile utils/arg_helper.ml
compile utils/profile.mli
compile utils/clflags.mli
compile utils/clflags.ml
compile utils/tbl.mli
compile utils/tbl.ml
compile utils/profile.ml
compile utils/terminfo.mli
compile utils/terminfo.ml
compile utils/ccomp.mli
compile utils/ccomp.ml
compile utils/warnings.mli
compile utils/warnings.ml
compile utils/consistbl.mli
compile utils/consistbl.ml
compile utils/strongly_connected_components.mli
compile utils/strongly_connected_components.ml
compile utils/build_path_prefix_map.mli
compile utils/build_path_prefix_map.ml
compile utils/targetint.mli
compile utils/targetint.ml
compile parsing/location.mli
compile parsing/location.ml
compile parsing/longident.mli
compile parsing/longident.ml
compile parsing/asttypes.mli
compile parsing/parsetree.mli
compile parsing/docstrings.mli
compile parsing/docstrings.ml
compile parsing/syntaxerr.mli
compile parsing/syntaxerr.ml
compile parsing/ast_helper.mli
compile parsing/ast_helper.ml
compile parsing/parser.mli
compile parsing/parser.ml
compile parsing/lexer.mli
compile parsing/lexer.ml
compile parsing/parse.mli
compile parsing/parse.ml
compile parsing/printast.mli
compile parsing/printast.ml
compile parsing/pprintast.mli
compile parsing/pprintast.ml
compile parsing/ast_mapper.mli
compile parsing/ast_mapper.ml
compile parsing/ast_iterator.mli
compile parsing/ast_iterator.ml
compile parsing/attr_helper.mli
compile parsing/attr_helper.ml
compile parsing/builtin_attributes.mli
compile parsing/builtin_attributes.ml
compile parsing/ast_invariants.mli
compile parsing/ast_invariants.ml
compile parsing/depend.mli
compile parsing/depend.ml
compile typing/ident.mli
compile typing/ident.ml
compile typing/path.mli
compile typing/path.ml
compile typing/outcometree.mli
compile typing/primitive.mli
compile typing/primitive.ml
compile typing/types.mli
compile typing/types.ml
compile typing/btype.mli
compile typing/btype.ml
compile typing/oprint.mli
compile typing/oprint.ml
compile typing/subst.mli
compile typing/subst.ml
compile typing/predef.mli
compile typing/predef.ml
compile typing/datarepr.mli
compile typing/datarepr.ml
compile typing/cmi_format.mli
compile typing/cmi_format.ml
compile typing/env.mli
compile typing/env.ml
compile typing/typedtree.mli
compile typing/typedtree.ml
compile typing/printtyped.mli
compile typing/printtyped.ml
compile typing/ctype.mli
compile typing/ctype.ml
compile typing/printtyp.mli
compile typing/printtyp.ml
compile typing/includeclass.mli
compile typing/includeclass.ml
compile typing/mtype.mli
compile typing/mtype.ml
compile typing/envaux.mli
compile typing/envaux.ml
compile typing/includecore.mli
compile typing/includecore.ml
compile typing/typedtreeIter.mli
compile typing/typedtreeIter.ml
compile typing/typedtreeMap.mli
compile typing/typedtreeMap.ml
compile typing/tast_mapper.mli
compile typing/tast_mapper.ml
compile typing/cmt_format.mli
compile typing/cmt_format.ml
compile typing/untypeast.mli
compile typing/untypeast.ml
compile typing/includemod.mli
compile typing/includemod.ml
compile typing/typetexp.mli
compile typing/typetexp.ml
compile typing/printpat.mli
compile typing/printpat.ml
compile typing/parmatch.mli
compile typing/parmatch.ml
compile typing/annot.mli
compile typing/stypes.mli
compile typing/stypes.ml
compile typing/typedecl.mli
compile typing/typedecl.ml
compile bytecomp/lambda.mli
compile typing/typeopt.mli
compile typing/typeopt.ml
compile typing/typecore.mli
compile typing/typecore.ml
compile typing/typeclass.mli
compile typing/typeclass.ml
compile typing/typemod.mli
compile typing/typemod.ml
compile bytecomp/lambda.ml
compile bytecomp/printlambda.mli
compile bytecomp/printlambda.ml
compile bytecomp/semantics_of_primitives.mli
compile bytecomp/semantics_of_primitives.ml
compile bytecomp/switch.mli
compile bytecomp/switch.ml
compile bytecomp/matching.mli
compile bytecomp/matching.ml
compile bytecomp/translobj.mli
compile bytecomp/translobj.ml
compile bytecomp/translattribute.mli
compile bytecomp/translattribute.ml
compile bytecomp/translprim.mli
compile bytecomp/translprim.ml
compile bytecomp/translcore.mli
compile bytecomp/translcore.ml
compile bytecomp/translclass.mli
compile bytecomp/translclass.ml
compile bytecomp/translmod.mli
compile bytecomp/translmod.ml
compile bytecomp/simplif.mli
compile bytecomp/simplif.ml
compile bytecomp/runtimedef.mli
compile bytecomp/runtimedef.ml
compile bytecomp/instruct.mli
compile bytecomp/meta.mli
compile bytecomp/meta.ml
compile bytecomp/opcodes.ml
compile bytecomp/bytesections.mli
compile bytecomp/bytesections.ml
compile bytecomp/dll.mli
compile bytecomp/dll.ml
compile bytecomp/cmo_format.mli
compile bytecomp/symtable.mli
compile bytecomp/symtable.ml
compile driver/pparse.mli
compile driver/pparse.ml
compile driver/main_args.mli
compile driver/main_args.ml
compile driver/compenv.mli
compile driver/compenv.ml
compile driver/compmisc.mli
compile driver/compmisc.ml
compile driver/compdynlink.mli
#compile -impl driver/compdynlink.mlbyte
compile -impl driver/compdynlink.mlno
compile driver/compplugin.mli
compile driver/compplugin.ml
compile driver/makedepend.mli
compile driver/makedepend.ml
echo $COMPILER -a -linkall -o compilerlibs/ocamlcommon.$AEXT utils/config.$EXT utils/misc.$EXT utils/identifiable.$EXT utils/numbers.$EXT utils/arg_helper.$EXT utils/clflags.$EXT utils/tbl.$EXT utils/profile.$EXT utils/terminfo.$EXT utils/ccomp.$EXT utils/warnings.$EXT utils/consistbl.$EXT utils/strongly_connected_components.$EXT utils/build_path_prefix_map.$EXT utils/targetint.$EXT parsing/location.$EXT parsing/longident.$EXT parsing/docstrings.$EXT parsing/syntaxerr.$EXT parsing/ast_helper.$EXT parsing/parser.$EXT parsing/lexer.$EXT parsing/parse.$EXT parsing/printast.$EXT parsing/pprintast.$EXT parsing/ast_mapper.$EXT parsing/ast_iterator.$EXT parsing/attr_helper.$EXT parsing/builtin_attributes.$EXT parsing/ast_invariants.$EXT parsing/depend.$EXT typing/ident.$EXT typing/path.$EXT typing/primitive.$EXT typing/types.$EXT typing/btype.$EXT typing/oprint.$EXT typing/subst.$EXT typing/predef.$EXT typing/datarepr.$EXT typing/cmi_format.$EXT typing/env.$EXT typing/typedtree.$EXT typing/printtyped.$EXT typing/ctype.$EXT typing/printtyp.$EXT typing/includeclass.$EXT typing/mtype.$EXT typing/envaux.$EXT typing/includecore.$EXT typing/typedtreeIter.$EXT typing/typedtreeMap.$EXT typing/tast_mapper.$EXT typing/cmt_format.$EXT typing/untypeast.$EXT typing/includemod.$EXT typing/typetexp.$EXT typing/printpat.$EXT typing/parmatch.$EXT typing/stypes.$EXT typing/typedecl.$EXT typing/typeopt.$EXT typing/typecore.$EXT typing/typeclass.$EXT typing/typemod.$EXT bytecomp/lambda.$EXT bytecomp/printlambda.$EXT bytecomp/semantics_of_primitives.$EXT bytecomp/switch.$EXT bytecomp/matching.$EXT bytecomp/translobj.$EXT bytecomp/translattribute.$EXT bytecomp/translprim.$EXT bytecomp/translcore.$EXT bytecomp/translclass.$EXT bytecomp/translmod.$EXT bytecomp/simplif.$EXT bytecomp/runtimedef.$EXT bytecomp/meta.$EXT bytecomp/opcodes.$EXT bytecomp/bytesections.$EXT bytecomp/dll.$EXT bytecomp/symtable.$EXT driver/pparse.$EXT driver/main_args.$EXT driver/compenv.$EXT driver/compmisc.$EXT driver/compdynlink.$EXT driver/compplugin.$EXT driver/makedepend.$EXT
$COMPILER -a -linkall -o compilerlibs/ocamlcommon.$AEXT utils/config.$EXT utils/misc.$EXT utils/identifiable.$EXT utils/numbers.$EXT utils/arg_helper.$EXT utils/clflags.$EXT utils/tbl.$EXT utils/profile.$EXT utils/terminfo.$EXT utils/ccomp.$EXT utils/warnings.$EXT utils/consistbl.$EXT utils/strongly_connected_components.$EXT utils/build_path_prefix_map.$EXT utils/targetint.$EXT parsing/location.$EXT parsing/longident.$EXT parsing/docstrings.$EXT parsing/syntaxerr.$EXT parsing/ast_helper.$EXT parsing/parser.$EXT parsing/lexer.$EXT parsing/parse.$EXT parsing/printast.$EXT parsing/pprintast.$EXT parsing/ast_mapper.$EXT parsing/ast_iterator.$EXT parsing/attr_helper.$EXT parsing/builtin_attributes.$EXT parsing/ast_invariants.$EXT parsing/depend.$EXT typing/ident.$EXT typing/path.$EXT typing/primitive.$EXT typing/types.$EXT typing/btype.$EXT typing/oprint.$EXT typing/subst.$EXT typing/predef.$EXT typing/datarepr.$EXT typing/cmi_format.$EXT typing/env.$EXT typing/typedtree.$EXT typing/printtyped.$EXT typing/ctype.$EXT typing/printtyp.$EXT typing/includeclass.$EXT typing/mtype.$EXT typing/envaux.$EXT typing/includecore.$EXT typing/typedtreeIter.$EXT typing/typedtreeMap.$EXT typing/tast_mapper.$EXT typing/cmt_format.$EXT typing/untypeast.$EXT typing/includemod.$EXT typing/typetexp.$EXT typing/printpat.$EXT typing/parmatch.$EXT typing/stypes.$EXT typing/typedecl.$EXT typing/typeopt.$EXT typing/typecore.$EXT typing/typeclass.$EXT typing/typemod.$EXT bytecomp/lambda.$EXT bytecomp/printlambda.$EXT bytecomp/semantics_of_primitives.$EXT bytecomp/switch.$EXT bytecomp/matching.$EXT bytecomp/translobj.$EXT bytecomp/translattribute.$EXT bytecomp/translprim.$EXT bytecomp/translcore.$EXT bytecomp/translclass.$EXT bytecomp/translmod.$EXT bytecomp/simplif.$EXT bytecomp/runtimedef.$EXT bytecomp/meta.$EXT bytecomp/opcodes.$EXT bytecomp/bytesections.$EXT bytecomp/dll.$EXT bytecomp/symtable.$EXT driver/pparse.$EXT driver/main_args.$EXT driver/compenv.$EXT driver/compmisc.$EXT driver/compdynlink.$EXT driver/compplugin.$EXT driver/makedepend.$EXT
compile bytecomp/instruct.ml
compile bytecomp/bytegen.mli
compile bytecomp/bytegen.ml
compile bytecomp/printinstr.mli
compile bytecomp/printinstr.ml
compile bytecomp/emitcode.mli
compile bytecomp/emitcode.ml
compile bytecomp/bytelink.mli
compile bytecomp/bytelink.ml
compile bytecomp/bytelibrarian.mli
compile bytecomp/bytelibrarian.ml
compile bytecomp/bytepackager.mli
compile bytecomp/bytepackager.ml
compile driver/errors.mli
compile driver/errors.ml
compile driver/compile.mli
compile driver/compile.ml
echo $COMPILER -a -o compilerlibs/ocamlbytecomp.$AEXT bytecomp/instruct.$EXT bytecomp/bytegen.$EXT bytecomp/printinstr.$EXT bytecomp/emitcode.$EXT bytecomp/bytelink.$EXT bytecomp/bytelibrarian.$EXT bytecomp/bytepackager.$EXT driver/errors.$EXT driver/compile.$EXT
$COMPILER -a -o compilerlibs/ocamlbytecomp.$AEXT bytecomp/instruct.$EXT bytecomp/bytegen.$EXT bytecomp/printinstr.$EXT bytecomp/emitcode.$EXT bytecomp/bytelink.$EXT bytecomp/bytelibrarian.$EXT bytecomp/bytepackager.$EXT driver/errors.$EXT driver/compile.$EXT
compile driver/main.mli
compile driver/main.ml
#echo $COMPILER  -compat-32 -o ocamlc compilerlibs/ocamlcommon.$AEXT compilerlibs/ocamlbytecomp.$AEXT driver/main.$EXT
#$COMPILER  -compat-32 -o ocamlc compilerlibs/ocamlcommon.$AEXT compilerlibs/ocamlbytecomp.$AEXT driver/main.$EXT
echo $COMPILER -o ocamlc compilerlibs/ocamlcommon.$AEXT compilerlibs/ocamlbytecomp.$AEXT driver/main.$EXT -cclib '"-lm -ldl -lpthread"'
$COMPILER -o ocamlc compilerlibs/ocamlcommon.$AEXT compilerlibs/ocamlbytecomp.$AEXT driver/main.$EXT -cclib "-lm -ldl -lpthread"
