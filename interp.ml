open Data
open Conf
open Eval
open Envir

let () = Runtime_lib.apply_ref := apply

let parse filename =
  let inc = open_in filename in
  let lexbuf = Lexing.from_channel inc in
  Location.init lexbuf filename;
  let parsed = Parse.implementation lexbuf in
  close_in inc;
  parsed

let z x = x

let stdlib_modules =
  [ ("Sys", "sys.ml", z);
    ("Seq", "seq.ml", z);
    ("List", "list.ml", z);
    ("Set", "set.ml", z);
    ("Map", "map.ml", z);
    ("Char", "char.ml", z);
    ("Bytes", "bytes.ml", z);
    ("String", "string.ml", z);
    ("Buffer", "buffer.ml", z);
    ("CamlinternalFormatBasics", "camlinternalFormatBasics.ml", z);
    ( "CamlinternalFormat",
      "camlinternalFormat.ml", z);
    ("Printf", "printf.ml", z);
    ("Format", "format.ml", z);
    ("Obj", "obj.ml", z);
    ("Gc", "gc.ml", z);
    ("CamlinternalLazy", "camlinternalLazy.ml", z);
    ("Lazy", "lazy.ml", z);
    ("Array", "array.ml", z);
    ("Int64", "int64.ml", z);
    ("Int32", "int32.ml", z);
    ("Nativeint", "nativeint.ml", z);
    ("Digest", "digest.ml", z);
    ("Random", "random.ml", z);
    ("Hashtbl", "hashtbl.ml", z);
    ("Lexing", "lexing.ml", z);
    ("Parsing", "parsing.ml", z);
    ("Weak", "weak.ml", z);
    ("Stack", "stack.ml", z);
    ("Arg", "arg.ml", z);
    ("Filename", "filename.ml", z);
    ("CamlinternalOO", "camlinternalOO.ml", z);
    ("Marshal", "marshal.ml", z)
  ]

let stdlib_modules =
  let stdlib_path = stdlib_path () in
  List.map
    (fun (n, p, modifier) -> (n, stdlib_path ^ "/" ^ p, modifier))
    stdlib_modules

let load_modules env modules =
  List.fold_left
    (fun env (modname, modpath, modifier) ->
      if debug then Format.eprintf "Loading %s from %s@." modname modpath;
      let module_contents =
        modifier (eval_structure None env (parse modpath))
      in
      env_set_module modname (make_module module_contents) env)
    env
    modules

let init_env =
  let stdlib_path = stdlib_path () in
  let stdlib_main = parse (stdlib_path ^ "/stdlib.ml") in
  let ign = ref SSet.empty in
  let env = eval_structure (Some ign) Primitives.initial_env stdlib_main in
  let env = load_modules env stdlib_modules in
  env_set_module "Stdlib" (make_module env) env

let compiler_modules =
  [ (* Utils *)
    ("Config", "utils/config.ml", z);
    ("Misc", "utils/misc.ml", z);
    ("Identifiable", "utils/identifiable.ml", z);
    ("Numbers", "utils/numbers.ml", z);
    ("Arg_helper", "utils/arg_helper.ml", z);
    ("Clflags", "utils/clflags.ml", z);
    ("Tbl", "utils/tbl.ml", z);
    ("Profile", "utils/profile.ml", z);
    ("Terminfo", "utils/terminfo.ml", z);
    ("Ccomp", "utils/ccomp.ml", z);
    ("Warnings", "utils/warnings.ml", z);
    ("Consistbl", "utils/consistbl.ml", z);
    ( "Strongly_connected_components",
      "utils/strongly_connected_components.ml",
      z );
    ("Build_path_prefix_map", "utils/build_path_prefix_map.ml", z);
    ("Targetint", "utils/targetint.ml", z);
    (* Parsing *)
    ("Asttypes", "parsing/asttypes.mli", z);
    ("Location", "parsing/location.ml", z);
    ("Longident", "parsing/longident.ml", z);
    ("Parsetree", "parsing/parsetree.mli", z);
    ("Docstrings", "parsing/docstrings.ml", z);
    ("Syntaxerr", "parsing/syntaxerr.ml", z);
    ("Ast_helper", "parsing/ast_helper.ml", z);
    ("Parser", "parsing/parser.ml", z);
    ("Lexer", "parsing/lexer.ml", z);
    ("Parse", "parsing/parse.ml", z);
    ("Printast", "parsing/printast.ml", z);
    ("Pprintast", "parsing/pprintast.ml", z);
    ("Ast_mapper", "parsing/ast_mapper.ml", z);
    ("Ast_iterator", "parsing/ast_iterator.ml", z);
    ("Attr_helper", "parsing/attr_helper.ml", z);
    ("Builtin_attributes", "parsing/builtin_attributes.ml", z);
    ("Ast_invariants", "parsing/ast_invariants.ml", z);
    ("Depend", "parsing/depend.ml", z);
    (* Typing *)
    ("Ident", "typing/ident.ml", z);
    ("Outcometree", "typing/outcometree.mli", z);
    ("Annot", "typing/annot.mli", z);
    ("Path", "typing/path.ml", z);
    ("Primitive", "typing/primitive.ml", z);
    ("Types", "typing/types.ml", z);
    ("Btype", "typing/btype.ml", z);
    ("Oprint", "typing/oprint.ml", z);
    ("Subst", "typing/subst.ml", z);
    ("Predef", "typing/predef.ml", z);
    ("Datarepr", "typing/datarepr.ml", z);
    ("Cmi_format", "typing/cmi_format.ml", z);
    ("Env", "typing/env.ml", z);
    ("Typedtree", "typing/typedtree.ml", z);
    ("Printtyped", "typing/printtyped.ml", z);
    ("Ctype", "typing/ctype.ml", z);
    ("Printtyp", "typing/printtyp.ml", z);
    ("Includeclass", "typing/includeclass.ml", z);
    ("Mtype", "typing/mtype.ml", z);
    ("Envaux", "typing/envaux.ml", z);
    ("Includecore", "typing/includecore.ml", z);
    ("TypedtreeIter", "typing/typedtreeIter.ml", z);
    ("TypedtreeMap", "typing/typedtreeMap.ml", z);
    ("Tast_mapper", "typing/tast_mapper.ml", z);
    ("Cmt_format", "typing/cmt_format.ml", z);
    ("Untypeast", "typing/untypeast.ml", z);
    ("Includemod", "typing/includemod.ml", z);
    ("Typetexp", "typing/typetexp.ml", z);
    ("Printpat", "typing/printpat.ml", z);
    ("Parmatch", "typing/parmatch.ml", z);
    ("Stypes", "typing/stypes.ml", z);
    ("Typedecl", "typing/typedecl.ml", z);
    (* Comp *)
    ("Lambda", "bytecomp/lambda.ml", z);
    (* Typing *)
    ("Typeopt", "typing/typeopt.ml", z);
    ("Typecore", "typing/typecore.ml", z);
    ("Typeclass", "typing/typeclass.ml", z);
    ("Typemod", "typing/typemod.ml", z);
    (* Comp *)
    ("Cmo_format", "bytecomp/cmo_format.mli", z);
    ("Printlambda", "bytecomp/printlambda.ml", z);
    ("Semantics_of_primitives", "bytecomp/semantics_of_primitives.ml", z);
    ("Switch", "bytecomp/switch.ml", z);
    ("Matching", "bytecomp/matching.ml", z);
    ("Translobj", "bytecomp/translobj.ml", z);
    ("Translattribute", "bytecomp/translattribute.ml", z);
    ("Translprim", "bytecomp/translprim.ml", z);
    ("Translcore", "bytecomp/translcore.ml", z);
    ("Translclass", "bytecomp/translclass.ml", z);
    ("Translmod", "bytecomp/translmod.ml", z);
    ("Simplif", "bytecomp/simplif.ml", z);
    ("Runtimedef", "bytecomp/runtimedef.ml", z);
    ("Meta", "bytecomp/meta.ml", z);
    ("Opcodes", "bytecomp/opcodes.ml", z);
    ("Bytesections", "bytecomp/bytesections.ml", z);
    ("Dll", "bytecomp/dll.ml", z);
    ("Symtable", "bytecomp/symtable.ml", z);
    ("Pparse", "driver/pparse.ml", z);
    ("Main_args", "driver/main_args.ml", z);
    ("Compenv", "driver/compenv.ml", z);
    ("Compmisc", "driver/compmisc.ml", z);
    ("Compdynlink", "driver/compdynlink.mlno", z);
    ("Compplugin", "driver/compplugin.ml", z);
    ("Makedepend", "driver/makedepend.ml", z);
    (* Bytecomp *)
    ("Instruct", "bytecomp/instruct.ml", z);
    ("Bytegen", "bytecomp/bytegen.ml", z);
    ("Printinstr", "bytecomp/printinstr.ml", z);
    ("Emitcode", "bytecomp/emitcode.ml", z);
    ("Bytelink", "bytecomp/bytelink.ml", z);
    ("Bytelibrarian", "bytecomp/bytelibrarian.ml", z);
    ("Bytepackager", "bytecomp/bytepackager.ml", z);
    ("Errors", "driver/errors.ml", z);
    ("Compile", "driver/compile.ml", z);
    (* Bytestart *)
    ("Main", "driver/main.ml", z)
  ]

let compiler_modules =
  let compiler_source_path = compiler_source_path () in
  List.map
    (fun (n, p, modifier) -> (n, compiler_source_path ^ "/" ^ p, modifier))
    compiler_modules

(* let _ = eval_structure None init_env parsed *)
let () =
  try ignore (load_modules init_env compiler_modules)
  with InternalException e ->
    Format.eprintf "Code raised exception: %a@." pp_print_value e

