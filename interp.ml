open Data
open Conf
open Eval
open Envir

let parse filename =
  let inc = open_in filename in
  let lexbuf = Lexing.from_channel inc in
  Location.init lexbuf filename;
  let parsed = Parse.implementation lexbuf in
  close_in inc;
  parsed

let stdlib_flag = [`Open (Longident.Lident "Stdlib")]
let no_stdlib_flag = []

let stdlib_units =
  let stdlib_path = stdlib_path () in
  let fullpath file = Filename.concat stdlib_path file in
  (no_stdlib_flag, fullpath "stdlib.ml")
  ::
  List.map (fun file -> stdlib_flag, fullpath file) [
    "sys.ml";
    "callback.ml";
    "complex.ml";
    "float.ml";
    "seq.ml";
    "list.ml";
    "listLabels.ml";
    "set.ml";
    "map.ml";
    "char.ml";
    "uchar.ml";
    "bytes.ml";
    "bytesLabels.ml";
    "string.ml";
    "stringLabels.ml";
    "buffer.ml";
    "stream.ml";
    "genlex.ml";
    "camlinternalFormatBasics.ml";
    "camlinternalFormat.ml";
    "printf.ml";
    "scanf.ml";
    "format.ml";
    "obj.ml";
    "gc.ml";
    "camlinternalOO.ml";
    "oo.ml";
    "camlinternalLazy.ml";
    "lazy.ml";
    "printexc.ml";
    "array.ml";
    "arrayLabels.ml";
    "sort.ml";
    "queue.ml";
    "int64.ml";
    "int32.ml";
    "nativeint.ml";
    "digest.ml";
    "random.ml";
    "hashtbl.ml";
    "lexing.ml";
    "parsing.ml";
    "weak.ml";
    "ephemeron.ml";
    "spacetime.ml";
    "stack.ml";
    "arg.ml";
    "filename.ml";
    "marshal.ml";
    "bigarray.ml";
    "moreLabels.ml";
    "stdLabels.ml";
  ]

let eval_env_flag ~loc env flag =
  match flag with
  | `Open module_ident ->
     let module_ident = Location.mkloc module_ident loc in
     env_extend false env (env_get_module_data env module_ident)

let load_rec_units env flags_and_units =
  let unit_paths = List.map snd flags_and_units in
  let env = List.fold_left declare_unit env unit_paths in
  List.fold_left
    (fun global_env (flags, unit_path) ->
      let module_name = module_name_of_unit_path unit_path in
      if debug then Format.eprintf "Loading %s from %s@." module_name unit_path;
      let module_contents =
        let loc = Location.in_file unit_path in
        let local_env = List.fold_left (eval_env_flag ~loc) global_env flags in
        eval_structure Primitives.prims local_env (parse unit_path)
      in
      define_unit global_env unit_path (make_module_data module_contents))
    env
    flags_and_units

let stdlib_env =
  let env = Runtime_base.initial_env in
  let env = load_rec_units env stdlib_units in
  env

let compiler_units =
  let compiler_source_path = compiler_source_path () in
  let fullpath file = Filename.concat compiler_source_path file in
  List.map (fun modfile -> stdlib_flag, fullpath modfile)
  [ (* Utils *)
    "utils/config.ml";
    "utils/misc.ml";
    "utils/identifiable.ml";
    "utils/numbers.ml";
    "utils/arg_helper.ml";
    "utils/clflags.ml";
    "utils/tbl.ml";
    "utils/profile.ml";
    "utils/terminfo.ml";
    "utils/ccomp.ml";
    "utils/warnings.ml";
    "utils/consistbl.ml";
    "utils/strongly_connected_components.ml";
    "utils/build_path_prefix_map.ml";
    "utils/targetint.ml";
    (* Parsing *)
    "parsing/asttypes.mli";
    "parsing/location.ml";
    "parsing/longident.ml";
    "parsing/parsetree.mli";
    "parsing/docstrings.ml";
    "parsing/syntaxerr.ml";
    "parsing/ast_helper.ml";
    "parsing/parser.ml";
    "parsing/lexer.ml";
    "parsing/parse.ml";
    "parsing/printast.ml";
    "parsing/pprintast.ml";
    "parsing/ast_mapper.ml";
    "parsing/ast_iterator.ml";
    "parsing/attr_helper.ml";
    "parsing/builtin_attributes.ml";
    "parsing/ast_invariants.ml";
    "parsing/depend.ml";
    (* Typing *)
    "typing/ident.ml";
    "typing/outcometree.mli";
    "typing/annot.mli";
    "typing/path.ml";
    "typing/primitive.ml";
    "typing/types.ml";
    "typing/btype.ml";
    "typing/oprint.ml";
    "typing/subst.ml";
    "typing/predef.ml";
    "typing/datarepr.ml";
    "typing/cmi_format.ml";
    "typing/env.ml";
    "typing/typedtree.ml";
    "typing/printtyped.ml";
    "typing/ctype.ml";
    "typing/printtyp.ml";
    "typing/includeclass.ml";
    "typing/mtype.ml";
    "typing/envaux.ml";
    "typing/includecore.ml";
    "typing/typedtreeIter.ml";
    "typing/typedtreeMap.ml";
    "typing/tast_mapper.ml";
    "typing/cmt_format.ml";
    "typing/untypeast.ml";
    "typing/includemod.ml";
    "typing/typetexp.ml";
    "typing/printpat.ml";
    "typing/parmatch.ml";
    "typing/stypes.ml";
    "typing/typedecl.ml";
    (* Comp *)
    "bytecomp/lambda.ml";
    (* Typing *)
    "typing/typeopt.ml";
    "typing/typecore.ml";
    "typing/typeclass.ml";
    "typing/typemod.ml";
    (* Comp *)
    "bytecomp/cmo_format.mli";
    "bytecomp/printlambda.ml";
    "bytecomp/semantics_of_primitives.ml";
    "bytecomp/switch.ml";
    "bytecomp/matching.ml";
    "bytecomp/translobj.ml";
    "bytecomp/translattribute.ml";
    "bytecomp/translprim.ml";
    "bytecomp/translcore.ml";
    "bytecomp/translclass.ml";
    "bytecomp/translmod.ml";
    "bytecomp/simplif.ml";
    "bytecomp/runtimedef.ml";
    "bytecomp/meta.ml";
    "bytecomp/opcodes.ml";
    "bytecomp/bytesections.ml";
    "bytecomp/dll.ml";
    "bytecomp/symtable.ml";
    "driver/pparse.ml";
    "driver/main_args.ml";
    "driver/compenv.ml";
    "driver/compmisc.ml";
    "driver/compdynlink.mlno";
    "driver/compplugin.ml";
    "driver/makedepend.ml";
    (* Bytecomp *)
    "bytecomp/instruct.ml";
    "bytecomp/bytegen.ml";
    "bytecomp/printinstr.ml";
    "bytecomp/emitcode.ml";
    "bytecomp/bytelink.ml";
    "bytecomp/bytelibrarian.ml";
    "bytecomp/bytepackager.ml";
    "driver/errors.ml";
    "driver/compile.ml";
    (* Bytestart *)
    "driver/main.ml"
  ]

(* let _ = load_rec_units stdlib_env [stdlib_flag, "test.ml"] *)
let () =
  try ignore (load_rec_units stdlib_env compiler_units)
  with InternalException e ->
    Format.eprintf "Code raised exception: %a@." pp_print_value e
