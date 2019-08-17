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

module Compiler_files = struct
  let utils = List.map (Filename.concat "utils") [
    "config.ml";
    "misc.ml";
    "identifiable.ml";
    "numbers.ml";
    "arg_helper.ml";
    "clflags.ml";
    "tbl.ml";
    "profile.ml";
    "terminfo.ml";
    "ccomp.ml";
    "warnings.ml";
    "consistbl.ml";
    "strongly_connected_components.ml";
    "build_path_prefix_map.ml";
    "targetint.ml";
  ]

  let parsing = List.map (Filename.concat "parsing") [
    "asttypes.mli";
    "location.ml";
    "longident.ml";
    "parsetree.mli";
    "docstrings.ml";
    "syntaxerr.ml";
    "ast_helper.ml";
    "parser.ml";
    "lexer.ml";
    "parse.ml";
    "printast.ml";
    "pprintast.ml";
    "ast_mapper.ml";
    "ast_iterator.ml";
    "attr_helper.ml";
    "builtin_attributes.ml";
    "ast_invariants.ml";
    "depend.ml";
  ]

  let pure_typing = List.map (Filename.concat "typing") [
    "ident.ml";
    "outcometree.mli";
    "annot.mli";
    "path.ml";
    "primitive.ml";
    "types.ml";
    "btype.ml";
    "oprint.ml";
    "subst.ml";
    "predef.ml";
    "datarepr.ml";
    "cmi_format.ml";
    "env.ml";
    "typedtree.ml";
    "printtyped.ml";
    "ctype.ml";
    "printtyp.ml";
    "includeclass.ml";
    "mtype.ml";
    "envaux.ml";
    "includecore.ml";
    "typedtreeIter.ml";
    "typedtreeMap.ml";
    "tast_mapper.ml";
    "cmt_format.ml";
    "untypeast.ml";
    "includemod.ml";
    "typetexp.ml";
    "printpat.ml";
    "parmatch.ml";
    "stypes.ml";
    "typedecl.ml";
  ]

  let lambda = List.map (Filename.concat "bytecomp") [
    "lambda.ml";
  ]

  let more_typing = List.map (Filename.concat "typing") [
    "typeopt.ml";
    "typecore.ml";
    "typeclass.ml";
    "typemod.ml";
  ]

  let bytecomp = List.map (Filename.concat "bytecomp") [
    "cmo_format.mli";
    "printlambda.ml";
    "semantics_of_primitives.ml";
    "switch.ml";
    "matching.ml";
    "translobj.ml";
    "translattribute.ml";
    "translprim.ml";
    "translcore.ml";
    "translclass.ml";
    "translmod.ml";
    "simplif.ml";
    "runtimedef.ml";
    "meta.ml";
    "opcodes.ml";
    "bytesections.ml";
    "dll.ml";
    "symtable.ml";
  ]

  let driver = List.map (Filename.concat "driver") [
    "pparse.ml";
    "main_args.ml";
    "compenv.ml";
    "compmisc.ml";
    "compdynlink.mlno";
    "compplugin.ml";
    "makedepend.ml";
  ]

  let middle_end = List.map (Filename.concat "middle_end") [
    "base_types/id_types.ml";
    "base_types/compilation_unit.ml";
    "base_types/set_of_closures_id.ml";
    "base_types/symbol.ml";
    "base_types/variable.ml";
    "base_types/closure_element.ml";
    "base_types/closure_id.ml";
    "base_types/var_within_closure.ml";
    "base_types/linkage_name.ml";
    "flambda_utils.ml";
    "simple_value_approx.ml";
    "debuginfo.ml";
  ]

  let asmcomp = List.map (Filename.concat "asmcomp") [
    "cmx_format.mli";
    "clambda.ml";
    "export_info.ml";
    "compilenv.ml";
    "import_approx.ml";

    "debug/reg_with_debug_info.ml";
    "debug/reg_availability_set.ml";
    "debug/available_regs.ml";

    "x86_ast.mli";
    "x86_proc.ml";
    "x86_dsl.ml";
    "x86_gas.ml";

    "arch.ml";
    "cmm.ml";
    "reg.ml";
    "mach.ml";
    "proc.ml";

    "selectgen.ml";
    "spacetime_profiling.ml";
    "selection.ml";

    "closure.ml";
    "strmatch.ml";
    "cmmgen.ml";
    "linearize.ml";
    "branch_relaxation.ml";
    "emitaux.ml";
    "emit.ml";
    "comballoc.ml";
    "CSEgen.ml";
    "CSE.ml";
    "liveness.ml";
    "deadcode.ml";
    "split.ml";
    "spill.ml";
    "interf.ml";
    "coloring.ml";
    "reloadgen.ml";
    "reload.ml";
    "schedgen.ml";
    "scheduling.ml";
    "asmgen.ml";
  ]

  let bytegen = List.map (Filename.concat "bytecomp") [
    "instruct.ml";
    "bytegen.ml";
    "printinstr.ml";
    "emitcode.ml";
    "bytelink.ml";
    "bytelibrarian.ml";
    "bytepackager.ml";
  ]

  let bytecode_main = List.map (Filename.concat "driver") [
    "errors.ml";
    "compile.ml";
    "main.ml";
  ]

  let native_main = List.map (Filename.concat "driver") [
    "opterrors.ml";
    "optcompile.ml";
    "optmain.ml";
  ]
end

let bytecode_compiler_units =
  let compiler_source_path = compiler_source_path () in
  let fullpath file = Filename.concat compiler_source_path file in
  List.map (fun modfile -> stdlib_flag, fullpath modfile)
  ( Compiler_files.utils
  @ Compiler_files.parsing
  @ Compiler_files.pure_typing
  @ Compiler_files.lambda
  @ Compiler_files.more_typing
  @ Compiler_files.bytecomp
  @ Compiler_files.driver
  @ Compiler_files.bytegen
  @ Compiler_files.bytecode_main
  )

let native_compiler_units =
  let compiler_source_path = compiler_source_path () in
  let fullpath file = Filename.concat compiler_source_path file in
  List.map (fun modfile -> stdlib_flag, fullpath modfile)
  ( Compiler_files.utils
  @ Compiler_files.parsing
  @ Compiler_files.pure_typing
  @ Compiler_files.lambda
  @ Compiler_files.more_typing
  @ Compiler_files.bytecomp
  @ Compiler_files.driver
  @ Compiler_files.middle_end
  @ Compiler_files.asmcomp
  @ Compiler_files.native_main
  )

(* let _ = load_rec_units stdlib_env [stdlib_flag, "test.ml"] *)
let () =
  try ignore (load_rec_units stdlib_env native_compiler_units)
  with InternalException e ->
    Format.eprintf "Code raised exception: %a@." pp_print_value e
