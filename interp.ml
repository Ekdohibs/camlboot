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

let module_name_of_path filename =
  filename
  |> Filename.basename
  |> Filename.remove_extension
  |> String.capitalize_ascii

let stdlib_modules =
  [ "sys.ml";
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
    "camlinternalOO.ml";
    "marshal.ml";
    "bigarray.ml";
    "moreLabels.ml";
    "stdLabels.ml";
    "stdlib.ml"
  ]

let stdlib_modules =
  let stdlib_path = stdlib_path () in
  List.map (fun p -> stdlib_path ^ "/" ^ p) stdlib_modules

let load_modules env modules =
  List.fold_left
    (fun env modpath ->
      let modname = module_name_of_path modpath in
      if debug then Format.eprintf "Loading %s from %s@." modname modpath;
      let module_contents =
        eval_structure None Primitives.prims env (parse modpath)
      in
      env_set_module modname (make_module module_contents) env)
    env
    modules

let init_env =
  let stdlib_path = stdlib_path () in
  let stdlib_main = parse (stdlib_path ^ "/stdlib.ml") in
  let ign = ref SSet.empty in
  let env =
    eval_structure
      (Some ign)
      Primitives.prims
      Runtime_base.initial_env
      stdlib_main
  in
  let env = load_modules env stdlib_modules in
  env_set_module "Stdlib" (make_module env) env

let compiler_modules =
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

let compiler_modules =
  let compiler_source_path = compiler_source_path () in
  List.map (fun p -> compiler_source_path ^ "/" ^ p) compiler_modules

(* let _ = eval_structure None init_env parsed *)
let () =
  try ignore (load_modules init_env compiler_modules)
  with InternalException e ->
    Format.eprintf "Code raised exception: %a@." pp_print_value e
