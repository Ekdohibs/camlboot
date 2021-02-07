#!/usr/bin/env bash

ulimit -s 2000000

export COMPILER=../../miniml/interp/interp
export COMPFLAGS="-strict-sequence -absname -w +a-4-9-41-42-44-45-48 -g -warn-error A -bin-annot -nostdlib -safe-string -strict-formats"

export EXT=cmx
export AEXT=cmxa

compile () {
		# Target = $1, source = $2
		echo $COMPILER $COMPFLAGS $(./Compflags $1) -o $1 -c $2
		$COMPILER $COMPFLAGS $(./Compflags $1) -o $1 -c $2
}

compile camlinternalFormatBasics.cmi camlinternalFormatBasics.mli
compile camlinternalFormatBasics.$EXT camlinternalFormatBasics.ml
compile stdlib.cmi stdlib.pp.mli
compile stdlib.$EXT stdlib.pp.ml
compile stdlib__seq.cmi seq.mli
compile stdlib__seq.$EXT seq.ml
compile stdlib__char.cmi char.mli
compile stdlib__char.$EXT char.ml
compile stdlib__uchar.cmi uchar.mli
compile stdlib__uchar.$EXT uchar.ml
compile stdlib__sys.cmi sys.mli
compile stdlib__sys.$EXT sys.ml
compile stdlib__list.cmi list.mli
compile stdlib__list.$EXT list.ml
compile stdlib__bytes.cmi bytes.mli
compile stdlib__bytes.$EXT bytes.ml
compile stdlib__string.cmi string.mli
compile stdlib__string.$EXT string.ml
compile stdlib__array.cmi array.mli
compile stdlib__sort.cmi sort.mli
compile stdlib__sort.$EXT sort.ml
compile stdlib__marshal.cmi marshal.mli
compile stdlib__marshal.$EXT marshal.ml
compile stdlib__int32.cmi int32.mli
compile stdlib__obj.cmi obj.mli
compile stdlib__obj.$EXT obj.ml
compile stdlib__float.cmi float.mli
compile stdlib__float.$EXT float.ml
compile stdlib__array.$EXT array.ml
compile stdlib__int32.$EXT int32.ml
compile stdlib__int64.cmi int64.mli
compile stdlib__int64.$EXT int64.ml
compile stdlib__nativeint.cmi nativeint.mli
compile stdlib__nativeint.$EXT nativeint.ml
compile stdlib__lexing.cmi lexing.mli
compile stdlib__lexing.$EXT lexing.ml
compile stdlib__parsing.cmi parsing.mli
compile stdlib__parsing.$EXT parsing.ml
compile stdlib__set.cmi set.mli
compile stdlib__set.$EXT set.ml
compile stdlib__map.cmi map.mli
compile stdlib__map.$EXT map.ml
compile stdlib__stack.cmi stack.mli
compile stdlib__stack.$EXT stack.ml
compile stdlib__queue.cmi queue.mli
compile stdlib__queue.$EXT queue.ml
compile camlinternalLazy.cmi camlinternalLazy.mli
compile camlinternalLazy.$EXT camlinternalLazy.ml
compile stdlib__lazy.cmi lazy.mli
compile stdlib__lazy.$EXT lazy.ml
compile stdlib__stream.cmi stream.mli
compile stdlib__stream.$EXT stream.ml
compile stdlib__buffer.cmi buffer.mli
compile stdlib__buffer.$EXT buffer.ml
compile camlinternalFormat.cmi camlinternalFormat.mli
compile camlinternalFormat.$EXT camlinternalFormat.ml
compile stdlib__printf.cmi printf.mli
compile stdlib__printf.$EXT printf.ml
compile stdlib__arg.cmi arg.mli
compile stdlib__arg.$EXT arg.ml
compile stdlib__printexc.cmi printexc.mli
compile stdlib__printexc.$EXT printexc.ml
compile stdlib__gc.cmi gc.mli
compile stdlib__gc.$EXT gc.ml
compile stdlib__digest.cmi digest.mli
compile stdlib__digest.$EXT digest.ml
compile stdlib__random.cmi random.mli
compile stdlib__random.$EXT random.ml
compile stdlib__hashtbl.cmi hashtbl.mli
compile stdlib__hashtbl.$EXT hashtbl.ml
compile stdlib__weak.cmi weak.mli
compile stdlib__weak.$EXT weak.ml
compile stdlib__format.cmi format.mli
compile stdlib__format.$EXT format.ml
compile stdlib__scanf.cmi scanf.mli
compile stdlib__scanf.$EXT scanf.ml
compile stdlib__callback.cmi callback.mli
compile stdlib__callback.$EXT callback.ml
compile camlinternalOO.cmi camlinternalOO.mli
compile camlinternalOO.$EXT camlinternalOO.ml
compile stdlib__oo.cmi oo.mli
compile stdlib__oo.$EXT oo.ml
compile camlinternalMod.cmi camlinternalMod.mli
compile camlinternalMod.$EXT camlinternalMod.ml
compile stdlib__genlex.cmi genlex.mli
compile stdlib__genlex.$EXT genlex.ml
compile stdlib__ephemeron.cmi ephemeron.mli
compile stdlib__ephemeron.$EXT ephemeron.ml
compile stdlib__filename.cmi filename.mli
compile stdlib__filename.$EXT filename.ml
compile stdlib__complex.cmi complex.mli
compile stdlib__complex.$EXT complex.ml
compile stdlib__arrayLabels.cmi arrayLabels.mli
compile stdlib__arrayLabels.$EXT arrayLabels.ml
compile stdlib__listLabels.cmi listLabels.mli
compile stdlib__listLabels.$EXT listLabels.ml
compile stdlib__bytesLabels.cmi bytesLabels.mli
compile stdlib__bytesLabels.$EXT bytesLabels.ml
compile stdlib__stringLabels.cmi stringLabels.mli
compile stdlib__stringLabels.$EXT stringLabels.ml
compile stdlib__moreLabels.cmi moreLabels.mli
compile stdlib__moreLabels.$EXT moreLabels.ml
compile stdlib__stdLabels.cmi stdLabels.mli
compile stdlib__stdLabels.$EXT stdLabels.ml
compile stdlib__spacetime.cmi spacetime.mli
compile stdlib__spacetime.$EXT spacetime.ml
compile stdlib__bigarray.cmi bigarray.mli
compile stdlib__bigarray.$EXT bigarray.ml
$COMPILER -a -o stdlib.$AEXT camlinternalFormatBasics.$EXT stdlib.$EXT stdlib__seq.$EXT stdlib__char.$EXT stdlib__uchar.$EXT stdlib__sys.$EXT stdlib__list.$EXT stdlib__bytes.$EXT stdlib__string.$EXT stdlib__sort.$EXT stdlib__marshal.$EXT stdlib__obj.$EXT stdlib__float.$EXT stdlib__array.$EXT stdlib__int32.$EXT stdlib__int64.$EXT stdlib__nativeint.$EXT stdlib__lexing.$EXT stdlib__parsing.$EXT stdlib__set.$EXT stdlib__map.$EXT stdlib__stack.$EXT stdlib__queue.$EXT camlinternalLazy.$EXT stdlib__lazy.$EXT stdlib__stream.$EXT stdlib__buffer.$EXT camlinternalFormat.$EXT stdlib__printf.$EXT stdlib__arg.$EXT stdlib__printexc.$EXT stdlib__gc.$EXT stdlib__digest.$EXT stdlib__random.$EXT stdlib__hashtbl.$EXT stdlib__weak.$EXT stdlib__format.$EXT stdlib__scanf.$EXT stdlib__callback.$EXT camlinternalOO.$EXT stdlib__oo.$EXT camlinternalMod.$EXT stdlib__genlex.$EXT stdlib__ephemeron.$EXT stdlib__filename.$EXT stdlib__complex.$EXT stdlib__arrayLabels.$EXT stdlib__listLabels.$EXT stdlib__bytesLabels.$EXT stdlib__stringLabels.$EXT stdlib__moreLabels.$EXT stdlib__stdLabels.$EXT stdlib__spacetime.$EXT stdlib__bigarray.$EXT
$COMPILER $COMPFLAGS -c std_exit.ml
