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

let stdlib_modules =
  [ ("Sys", "sys.ml");
    ("Callback", "callback.ml");
    ("Complex", "complex.ml");
    ("Float", "float.ml");
    ("Seq", "seq.ml");
    ("List", "list.ml");
    ("ListLabels", "listLabels.ml");
    ("Set", "set.ml");
    ("Map", "map.ml");
    ("Char", "char.ml");
    ("Uchar", "uchar.ml");
    ("Bytes", "bytes.ml");
    ("BytesLabels", "bytesLabels.ml");
    ("String", "string.ml");
    ("StringLabels", "stringLabels.ml");
    ("Buffer", "buffer.ml");
    ("Stream", "stream.ml");
    ("Genlex", "genlex.ml");
    ("CamlinternalFormatBasics", "camlinternalFormatBasics.ml");
    ("CamlinternalFormat", "camlinternalFormat.ml");
    ("Printf", "printf.ml");
    ("Scanf", "scanf.ml");
    ("Format", "format.ml");
    ("Obj", "obj.ml");
    ("Gc", "gc.ml");
    ("CamlinternalOO", "camlinternalOO.ml");
    ("Oo", "oo.ml");
    ("CamlinternalLazy", "camlinternalLazy.ml");
    ("Lazy", "lazy.ml");
    ("Printexc", "printexc.ml");
    ("Array", "array.ml");
    ("ArrayLabels", "arrayLabels.ml");
    ("Sort", "sort.ml");
    ("Queue", "queue.ml");
    ("Int64", "int64.ml");
    ("Int32", "int32.ml");
    ("Nativeint", "nativeint.ml");
    ("Digest", "digest.ml");
    ("Random", "random.ml");
    ("Hashtbl", "hashtbl.ml");
    ("Lexing", "lexing.ml");
    ("Parsing", "parsing.ml");
    ("Weak", "weak.ml");
    ("Ephemeron", "ephemeron.ml");
    ("Spacetime", "spacetime.ml");
    ("Stack", "stack.ml");
    ("Arg", "arg.ml");
    ("Filename", "filename.ml");
    ("CamlinternalOO", "camlinternalOO.ml");
    ("Marshal", "marshal.ml");
    ("Bigarray", "bigarray.ml");
    ("MoreLabels", "moreLabels.ml");
    ("StdLabels", "stdLabels.ml");
    ("Stdlib", "stdlib.ml");
  ]

let stdlib_modules =
  let stdlib_path = stdlib_path () in
  List.map
    (fun (n, p) -> (n, stdlib_path ^ "/" ^ p))
    stdlib_modules

let load_modules env modules =
  List.fold_left
    (fun env (modname, modpath) ->
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
    ("Config", "utils/config.ml");
    ("Misc", "utils/misc.ml");
    ("Identifiable", "utils/identifiable.ml");
    ("Numbers", "utils/numbers.ml");
    ("Arg_helper", "utils/arg_helper.ml");
    ("Clflags", "utils/clflags.ml");
    ("Tbl", "utils/tbl.ml");
    ("Profile", "utils/profile.ml");
    ("Terminfo", "utils/terminfo.ml");
    ("Ccomp", "utils/ccomp.ml");
    ("Warnings", "utils/warnings.ml");
    ("Consistbl", "utils/consistbl.ml");
    ( "Strongly_connected_components",
      "utils/strongly_connected_components.ml" );
    ("Build_path_prefix_map", "utils/build_path_prefix_map.ml");
    ("Targetint", "utils/targetint.ml");
    (* Parsing *)
    ("Asttypes", "parsing/asttypes.mli");
    ("Location", "parsing/location.ml");
    ("Longident", "parsing/longident.ml");
    ("Parsetree", "parsing/parsetree.mli");
    ("Docstrings", "parsing/docstrings.ml");
    ("Syntaxerr", "parsing/syntaxerr.ml");
    ("Ast_helper", "parsing/ast_helper.ml");
    ("Parser", "parsing/parser.ml");
    ("Lexer", "parsing/lexer.ml");
    ("Parse", "parsing/parse.ml");
    ("Printast", "parsing/printast.ml");
    ("Pprintast", "parsing/pprintast.ml");
    ("Ast_mapper", "parsing/ast_mapper.ml");
    ("Ast_iterator", "parsing/ast_iterator.ml");
    ("Attr_helper", "parsing/attr_helper.ml");
    ("Builtin_attributes", "parsing/builtin_attributes.ml");
    ("Ast_invariants", "parsing/ast_invariants.ml");
    ("Depend", "parsing/depend.ml");
    (* Typing *)
    ("Ident", "typing/ident.ml");
    ("Outcometree", "typing/outcometree.mli");
    ("Annot", "typing/annot.mli");
    ("Path", "typing/path.ml");
    ("Primitive", "typing/primitive.ml");
    ("Types", "typing/types.ml");
    ("Btype", "typing/btype.ml");
    ("Oprint", "typing/oprint.ml");
    ("Subst", "typing/subst.ml");
    ("Predef", "typing/predef.ml");
    ("Datarepr", "typing/datarepr.ml");
    ("Cmi_format", "typing/cmi_format.ml");
    ("Env", "typing/env.ml");
    ("Typedtree", "typing/typedtree.ml");
    ("Printtyped", "typing/printtyped.ml");
    ("Ctype", "typing/ctype.ml");
    ("Printtyp", "typing/printtyp.ml");
    ("Includeclass", "typing/includeclass.ml");
    ("Mtype", "typing/mtype.ml");
    ("Envaux", "typing/envaux.ml");
    ("Includecore", "typing/includecore.ml");
    ("TypedtreeIter", "typing/typedtreeIter.ml");
    ("TypedtreeMap", "typing/typedtreeMap.ml");
    ("Tast_mapper", "typing/tast_mapper.ml");
    ("Cmt_format", "typing/cmt_format.ml");
    ("Untypeast", "typing/untypeast.ml");
    ("Includemod", "typing/includemod.ml");
    ("Typetexp", "typing/typetexp.ml");
    ("Printpat", "typing/printpat.ml");
    ("Parmatch", "typing/parmatch.ml");
    ("Stypes", "typing/stypes.ml");
    ("Typedecl", "typing/typedecl.ml");
    (* Comp *)
    ("Lambda", "bytecomp/lambda.ml");
    (* Typing *)
    ("Typeopt", "typing/typeopt.ml");
    ("Typecore", "typing/typecore.ml");
    ("Typeclass", "typing/typeclass.ml");
    ("Typemod", "typing/typemod.ml");
    (* Comp *)
    ("Cmo_format", "bytecomp/cmo_format.mli");
    ("Printlambda", "bytecomp/printlambda.ml");
    ("Semantics_of_primitives", "bytecomp/semantics_of_primitives.ml");
    ("Switch", "bytecomp/switch.ml");
    ("Matching", "bytecomp/matching.ml");
    ("Translobj", "bytecomp/translobj.ml");
    ("Translattribute", "bytecomp/translattribute.ml");
    ("Translprim", "bytecomp/translprim.ml");
    ("Translcore", "bytecomp/translcore.ml");
    ("Translclass", "bytecomp/translclass.ml");
    ("Translmod", "bytecomp/translmod.ml");
    ("Simplif", "bytecomp/simplif.ml");
    ("Runtimedef", "bytecomp/runtimedef.ml");
    ("Meta", "bytecomp/meta.ml");
    ("Opcodes", "bytecomp/opcodes.ml");
    ("Bytesections", "bytecomp/bytesections.ml");
    ("Dll", "bytecomp/dll.ml");
    ("Symtable", "bytecomp/symtable.ml");
    ("Pparse", "driver/pparse.ml");
    ("Main_args", "driver/main_args.ml");
    ("Compenv", "driver/compenv.ml");
    ("Compmisc", "driver/compmisc.ml");
    ("Compdynlink", "driver/compdynlink.mlno");
    ("Compplugin", "driver/compplugin.ml");
    ("Makedepend", "driver/makedepend.ml");
    (* Bytecomp *)
    ("Instruct", "bytecomp/instruct.ml");
    ("Bytegen", "bytecomp/bytegen.ml");
    ("Printinstr", "bytecomp/printinstr.ml");
    ("Emitcode", "bytecomp/emitcode.ml");
    ("Bytelink", "bytecomp/bytelink.ml");
    ("Bytelibrarian", "bytecomp/bytelibrarian.ml");
    ("Bytepackager", "bytecomp/bytepackager.ml");
    ("Errors", "driver/errors.ml");
    ("Compile", "driver/compile.ml");
    (* Bytestart *)
    ("Main", "driver/main.ml");
  ]

let compiler_modules =
  let compiler_source_path = compiler_source_path () in
  List.map
    (fun (n, p) -> (n, compiler_source_path ^ "/" ^ p))
    compiler_modules

(* let _ = eval_structure None init_env parsed *)
let () =
  try ignore (load_modules init_env compiler_modules)
  with InternalException e ->
    Format.eprintf "Code raised exception: %a@." pp_print_value e
