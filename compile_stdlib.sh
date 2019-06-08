#!/usr/bin/env bash

ulimit -s 2000000

export COMPILER=../../miniml/interp/interp
export COMPFLAGS="-strict-sequence -absname -w +a-4-9-41-42-44-45-48 -g -warn-error A -bin-annot -nostdlib -safe-string -strict-formats"

compile () {
		# Target = $1, source = $2
		echo $COMPILER $COMPFLAGS $(./Compflags $1) -o $1 -c $2
		$COMPILER $COMPFLAGS $(./Compflags $1) -o $1 -c $2
}

compile camlinternalFormatBasics.cmi camlinternalFormatBasics.mli
compile camlinternalFormatBasics.cmo camlinternalFormatBasics.ml
compile stdlib.cmi stdlib.pp.mli
compile stdlib.cmo stdlib.pp.ml
compile stdlib__seq.cmi seq.mli
compile stdlib__seq.cmo seq.ml
compile stdlib__char.cmi char.mli
compile stdlib__char.cmo char.ml
compile stdlib__uchar.cmi uchar.mli
compile stdlib__uchar.cmo uchar.ml
compile stdlib__sys.cmi sys.mli
compile stdlib__sys.cmo sys.ml
compile stdlib__list.cmi list.mli
compile stdlib__list.cmo list.ml
compile stdlib__bytes.cmi bytes.mli
compile stdlib__bytes.cmo bytes.ml
compile stdlib__string.cmi string.mli
compile stdlib__string.cmo string.ml
compile stdlib__array.cmi array.mli
compile stdlib__sort.cmi sort.mli
compile stdlib__sort.cmo sort.ml
compile stdlib__marshal.cmi marshal.mli
compile stdlib__marshal.cmo marshal.ml
compile stdlib__int32.cmi int32.mli
compile stdlib__obj.cmi obj.mli
compile stdlib__obj.cmo obj.ml
compile stdlib__float.cmi float.mli
compile stdlib__float.cmo float.ml
compile stdlib__array.cmo array.ml
compile stdlib__int32.cmo int32.ml
compile stdlib__int64.cmi int64.mli
compile stdlib__int64.cmo int64.ml
compile stdlib__nativeint.cmi nativeint.mli
compile stdlib__nativeint.cmo nativeint.ml
compile stdlib__lexing.cmi lexing.mli
compile stdlib__lexing.cmo lexing.ml
compile stdlib__parsing.cmi parsing.mli
compile stdlib__parsing.cmo parsing.ml
compile stdlib__set.cmi set.mli
compile stdlib__set.cmo set.ml
compile stdlib__map.cmi map.mli
compile stdlib__map.cmo map.ml
compile stdlib__stack.cmi stack.mli
compile stdlib__stack.cmo stack.ml
compile stdlib__queue.cmi queue.mli
compile stdlib__queue.cmo queue.ml
compile camlinternalLazy.cmi camlinternalLazy.mli
compile camlinternalLazy.cmo camlinternalLazy.ml
compile stdlib__lazy.cmi lazy.mli
compile stdlib__lazy.cmo lazy.ml
compile stdlib__stream.cmi stream.mli
compile stdlib__stream.cmo stream.ml
compile stdlib__buffer.cmi buffer.mli
compile stdlib__buffer.cmo buffer.ml
compile camlinternalFormat.cmi camlinternalFormat.mli
compile camlinternalFormat.cmo camlinternalFormat.ml
compile stdlib__printf.cmi printf.mli
compile stdlib__printf.cmo printf.ml
compile stdlib__arg.cmi arg.mli
compile stdlib__arg.cmo arg.ml
compile stdlib__printexc.cmi printexc.mli
compile stdlib__printexc.cmo printexc.ml
compile stdlib__gc.cmi gc.mli
compile stdlib__gc.cmo gc.ml
compile stdlib__digest.cmi digest.mli
compile stdlib__digest.cmo digest.ml
compile stdlib__random.cmi random.mli
compile stdlib__random.cmo random.ml
compile stdlib__hashtbl.cmi hashtbl.mli
compile stdlib__hashtbl.cmo hashtbl.ml
compile stdlib__weak.cmi weak.mli
compile stdlib__weak.cmo weak.ml
compile stdlib__format.cmi format.mli
compile stdlib__format.cmo format.ml
compile stdlib__scanf.cmi scanf.mli
compile stdlib__scanf.cmo scanf.ml
compile stdlib__callback.cmi callback.mli
compile stdlib__callback.cmo callback.ml
compile camlinternalOO.cmi camlinternalOO.mli
compile camlinternalOO.cmo camlinternalOO.ml
compile stdlib__oo.cmi oo.mli
compile stdlib__oo.cmo oo.ml
compile camlinternalMod.cmi camlinternalMod.mli
compile camlinternalMod.cmo camlinternalMod.ml
compile stdlib__genlex.cmi genlex.mli
compile stdlib__genlex.cmo genlex.ml
compile stdlib__ephemeron.cmi ephemeron.mli
compile stdlib__ephemeron.cmo ephemeron.ml
compile stdlib__filename.cmi filename.mli
compile stdlib__filename.cmo filename.ml
compile stdlib__complex.cmi complex.mli
compile stdlib__complex.cmo complex.ml
compile stdlib__arrayLabels.cmi arrayLabels.mli
compile stdlib__arrayLabels.cmo arrayLabels.ml
compile stdlib__listLabels.cmi listLabels.mli
compile stdlib__listLabels.cmo listLabels.ml
compile stdlib__bytesLabels.cmi bytesLabels.mli
compile stdlib__bytesLabels.cmo bytesLabels.ml
compile stdlib__stringLabels.cmi stringLabels.mli
compile stdlib__stringLabels.cmo stringLabels.ml
compile stdlib__moreLabels.cmi moreLabels.mli
compile stdlib__moreLabels.cmo moreLabels.ml
compile stdlib__stdLabels.cmi stdLabels.mli
compile stdlib__stdLabels.cmo stdLabels.ml
compile stdlib__spacetime.cmi spacetime.mli
compile stdlib__spacetime.cmo spacetime.ml
compile stdlib__bigarray.cmi bigarray.mli
compile stdlib__bigarray.cmo bigarray.ml
$COMPILER -a -o stdlib.cma camlinternalFormatBasics.cmo stdlib.cmo stdlib__seq.cmo stdlib__char.cmo stdlib__uchar.cmo stdlib__sys.cmo stdlib__list.cmo stdlib__bytes.cmo stdlib__string.cmo stdlib__sort.cmo stdlib__marshal.cmo stdlib__obj.cmo stdlib__float.cmo stdlib__array.cmo stdlib__int32.cmo stdlib__int64.cmo stdlib__nativeint.cmo stdlib__lexing.cmo stdlib__parsing.cmo stdlib__set.cmo stdlib__map.cmo stdlib__stack.cmo stdlib__queue.cmo camlinternalLazy.cmo stdlib__lazy.cmo stdlib__stream.cmo stdlib__buffer.cmo camlinternalFormat.cmo stdlib__printf.cmo stdlib__arg.cmo stdlib__printexc.cmo stdlib__gc.cmo stdlib__digest.cmo stdlib__random.cmo stdlib__hashtbl.cmo stdlib__weak.cmo stdlib__format.cmo stdlib__scanf.cmo stdlib__callback.cmo camlinternalOO.cmo stdlib__oo.cmo camlinternalMod.cmo stdlib__genlex.cmo stdlib__ephemeron.cmo stdlib__filename.cmo stdlib__complex.cmo stdlib__arrayLabels.cmo stdlib__listLabels.cmo stdlib__bytesLabels.cmo stdlib__stringLabels.cmo stdlib__moreLabels.cmo stdlib__stdLabels.cmo stdlib__spacetime.cmo stdlib__bigarray.cmo
$COMPILER $COMPFLAGS -c std_exit.ml
