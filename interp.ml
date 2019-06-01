open Asttypes
open Parsetree

open Conf
open Data
open Envir

exception Match_fail

let rec lident_name = function
  | Longident.Lident s -> s
  | Longident.Ldot (_, s) -> s
  | Longident.Lapply (_l1, l2) -> lident_name l2

let rec eval_fun_or_function envref expr =
  match expr.pexp_desc with
  | Pexp_function cl -> Function (cl, envref)
  | Pexp_fun (label, default, p, e) -> Fun (label, default, p, e, envref)
  | Pexp_constraint (e, _) | Pexp_coerce (e, _, _) | Pexp_newtype (_, e) ->
    eval_fun_or_function envref e
  | _ -> failwith "unsupported rhs of rec"

let rec expr_label_shape = function
  | Pexp_fun (label, default, _, e) ->
    (label, default) :: expr_label_shape e.pexp_desc
  | Pexp_function _ -> [ (Nolabel, None) ]
  | _ -> []

let fun_label_shape = function
  | Fun (lab, default, _, e, _) ->
    (lab, default) :: expr_label_shape e.pexp_desc
  | Function _ -> [ (Nolabel, None) ]
  | Prim _ -> [ (Nolabel, None) ]
  | SeqOr | SeqAnd -> [ (Nolabel, None); (Nolabel, None) ]
  | _ -> []

(*
let rec expr_num_args = function
  | Pexp_fun (_, _, _, e) -> 1 + expr_num_args e.pexp_desc
  | Pexp_function _ -> 1
  | _ -> 0

let rec fun_num_args = function
  | Fun (_, _, _, e, _) -> 1 + expr_num_args e.pexp_desc
  | Function _ -> 1
  | Prim _ -> 1
  | SeqOr | SeqAnd -> 2
  | Fun_with_extra_args (f, l, m) -> fun_num_args f - List.length l - SMap.cardinal m
  | _ -> 0
*)

module R = Runtime_stdlib

let rec apply vf args =
  let vf, extral, extram =
    match vf with
    | Fun_with_extra_args (vf, extral, extram) -> (vf, extral, extram)
    | _ -> (vf, [], SMap.empty)
  in
  assert (extral = []);
  (* let ls = fun_label_shape vf in *)
  let apply_labelled vf (lab, arg) =
    match vf with
    | Fun (label, default, p, e, fenv) ->
      (match (label, lab, default) with
      | Optional s, Labelled s', None ->
        assert (s = s');
        eval_expr (pattern_bind !fenv p (Constructor ("Some", 0, Some arg))) e
      | Optional s, Labelled s', Some _
      | Optional s, Optional s', None
      | Labelled s, Labelled s', None ->
        assert (s = s');
        eval_expr (pattern_bind !fenv p arg) e
      | Optional s, Optional s', Some def ->
        assert (s = s');
        let arg =
          match arg with
          | Constructor ("None", 0, None) -> eval_expr !fenv def
          | Constructor ("Some", 0, Some arg) -> arg
          | _ -> assert false
        in
        eval_expr (pattern_bind !fenv p arg) e
      | _ -> assert false)
    | _ -> assert false
  in
  let apply_optional_noarg vf =
    match vf with
    | Fun (Optional _, None, p, e, fenv) ->
      eval_expr (pattern_bind !fenv p (Constructor ("None", 0, None))) e
    | Fun (Optional _, Some def, p, e, fenv) ->
      eval_expr (pattern_bind !fenv p (eval_expr !fenv def)) e
    | _ -> assert false
  in
  let unlabelled =
    List.map snd (List.filter (fun (lab, _) -> lab = Nolabel) args)
  in
  let with_label =
    ref
      (List.fold_left
         (fun wl (lab, arg) ->
           match lab with
           | Nolabel -> wl
           | Optional s | Labelled s -> SMap.add s (lab, arg) wl)
         extram
         args)
  in
  let has_labelled = not (SMap.is_empty !with_label) in
  let rec apply_one vf arg =
    match vf with
    | Fun (Nolabel, _default, p, e, fenv) ->
      eval_expr (pattern_bind !fenv p arg) e
    | Fun (((Labelled s | Optional s) as lab), _default, p, e, fenv) ->
      if has_labelled
      then
        if SMap.mem s !with_label
        then (
          let v = SMap.find s !with_label in
          with_label := SMap.remove s !with_label;
          apply_one (apply_labelled vf v) arg)
        else (
          assert (lab = Optional s);
          apply_one (apply_optional_noarg vf) arg)
      else if lab = Optional s
      then apply_one (apply_optional_noarg vf) arg
      else eval_expr (pattern_bind !fenv p arg) e
    | Function (cl, fenv) -> eval_match !fenv cl (Ok arg)
    | Prim prim -> prim arg
    | SeqOr ->
      if is_true arg
      then Prim (fun _ -> R.wrap_bool true)
      else Prim (fun x -> x)
    | SeqAnd ->
      if is_true arg
      then Prim (fun x -> x)
      else Prim (fun _ -> R.wrap_bool false)
    | v ->
      Format.eprintf "%a@." pp_print_value v;
      assert false
  in
  if SMap.is_empty !with_label
  then
    (* Special case to get tail recursion *)
    List.fold_left apply_one vf unlabelled
  else (
    let vf = List.fold_left apply_one vf unlabelled in
    let rec apply_loop vf =
      if SMap.is_empty !with_label
      then vf
      else (
        match vf with
        | Fun (((Labelled s | Optional s) as lab), _default, _p, _e, _fenv) ->
          if SMap.mem s !with_label
          then (
            let v = SMap.find s !with_label in
            with_label := SMap.remove s !with_label;
            apply_loop (apply_labelled vf v))
          else (
            assert (lab = Optional s);
            apply_loop (apply_optional_noarg vf))
        | _ -> Fun_with_extra_args (vf, [], !with_label))
    in
    apply_loop vf)

and eval_expr env expr =
  match expr.pexp_desc with
  | Pexp_ident id -> env_get_value env id
  | Pexp_constant c -> value_of_constant c
  | Pexp_let (f, vals, e) ->
    if f = Nonrecursive
    then (
      let nenv = List.fold_left (bind_value env) env vals in
      eval_expr nenv e)
    else (
      let er = ref env in
      let nenv = List.fold_left (bind_value_rec er) env vals in
      er := nenv;
      eval_expr nenv e)
  | Pexp_function cl -> Function (cl, ref env)
  | Pexp_fun (label, default, p, e) -> Fun (label, default, p, e, ref env)
  | Pexp_apply (f, l) ->
    let fc = eval_expr env f in
    (match (fc, l) with
    | SeqOr, [ (_, arg1); (_, arg2) ] ->
      let a1 = eval_expr env arg1 in
      if is_true a1 then R.wrap_bool true else eval_expr env arg2
    | SeqAnd, [ (_, arg1); (_, arg2) ] ->
      let a1 = eval_expr env arg1 in
      if is_true a1 then eval_expr env arg2 else R.wrap_bool false
    | _ ->
      let args = List.map (fun (lab, e) -> (lab, eval_expr env e)) l in
      if trace
      then (
        match f.pexp_desc with
        | Pexp_ident lident ->
          Format.eprintf
            "apply %s"
            (String.concat "." (Longident.flatten lident.txt));
          incr tracecur;
          if !tracecur > tracearg_from
          then
            Format.eprintf
              " %a"
              (Format.pp_print_list
                 ~pp_sep:(fun ff () -> Format.fprintf ff " ")
                 (fun ff (_, v) -> Format.fprintf ff "%a" pp_print_value v))
              args;
          Format.eprintf "@."
        | _ -> ());
      apply fc args)
  | Pexp_tuple l ->
    let args = List.map (eval_expr env) l in
    Tuple args
  | Pexp_match (e, cl) -> eval_match env cl (eval_expr_exn env e)
  | Pexp_coerce (e, _, _) -> eval_expr env e
  | Pexp_constraint (e, _) -> eval_expr env e
  | Pexp_sequence (e1, e2) ->
    let _ = eval_expr env e1 in
    eval_expr env e2
  | Pexp_while (e1, e2) ->
    while is_true (eval_expr env e1) do
      ignore (eval_expr env e2)
    done;
    unit
  | Pexp_for (p, e1, e2, flag, e3) ->
    let v1 =
      match eval_expr env e1 with
      | Int n -> n
      | _ -> assert false
    in
    let v2 =
      match eval_expr env e2 with
      | Int n -> n
      | _ -> assert false
    in
    if flag = Upto
    then
      for x = v1 to v2 do
        ignore (eval_expr (pattern_bind env p (Int x)) e3)
      done
    else
      for x = v1 downto v2 do
        ignore (eval_expr (pattern_bind env p (Int x)) e3)
      done;
    unit
  | Pexp_ifthenelse (e1, e2, e3) ->
    if is_true (eval_expr env e1)
    then eval_expr env e2
    else (
      match e3 with
      | None -> unit
      | Some e3 -> eval_expr env e3)
  | Pexp_unreachable -> failwith "reached unreachable"
  | Pexp_try (e, cs) ->
    (try eval_expr env e
     with InternalException v ->
       (try eval_match env cs (Ok v)
        with Match_fail -> raise (InternalException v)))
  | Pexp_construct (c, e) ->
    let cn = lident_name c.txt in
    let d = env_get_constr env c in
    let ee =
      match e with
      | None -> None
      | Some e -> Some (eval_expr env e)
    in
    Constructor (cn, d, ee)
  | Pexp_variant (cn, e) ->
    let ee =
      match e with
      | None -> None
      | Some e -> Some (eval_expr env e)
    in
    Constructor (cn, Hashtbl.hash cn, ee)
  | Pexp_record (r, e) ->
    let base =
      match e with
      | None -> SMap.empty
      | Some e ->
        (match eval_expr env e with
        | Record r -> r
        | _ -> assert false)
    in
    Record
      (List.fold_left
         (fun rc ({ txt = lident; _ }, ee) ->
           SMap.add (lident_name lident) (ref (eval_expr env ee)) rc)
         base
         r)
  | Pexp_field (e, { txt = lident; _ }) ->
    (match eval_expr env e with
    | Record r -> !(SMap.find (lident_name lident) r)
    | _ -> assert false)
  | Pexp_setfield (e1, { txt = lident; _ }, e2) ->
    let v1 = eval_expr env e1 in
    let v2 = eval_expr env e2 in
    (match v1 with
    | Record r ->
      SMap.find (lident_name lident) r := v2;
      unit
    | _ -> assert false)
  | Pexp_array l -> Array (Array.of_list (List.map (eval_expr env) l))
  | Pexp_send _ -> assert false
  | Pexp_new _ -> assert false
  | Pexp_setinstvar _ -> assert false
  | Pexp_override _ -> assert false
  | Pexp_letexception ({ pext_name = name; pext_kind = k; _ }, e) ->
    let nenv =
      match k with
      | Pext_decl _ ->
        let d = Primitives.next_exn_id () in
        env_set_constr name.txt d env
      | Pext_rebind path ->
        env_set_constr name.txt (env_get_constr env path) env
    in
    eval_expr nenv e
  | Pexp_letmodule (name, me, e) ->
    let m = eval_module_expr env me in
    eval_expr (env_set_module name.txt m env) e
  | Pexp_assert e ->
    if is_true (eval_expr env e)
    then unit
    else
      (*failwith "assert failure"*)
      raise
        (InternalException
           (Constructor
              ( "Assert_failure",
                Primitives.assert_failure_id,
                Some (Tuple [ R.wrap_string ""; Int 0; Int 0 ]) )))
  | Pexp_lazy e -> Lz (ref (fun () -> eval_expr env e))
  | Pexp_poly _ -> assert false
  | Pexp_newtype (_, e) -> eval_expr env e
  | Pexp_open (_, lident, e) ->
    let nenv =
      match env_get_module env lident with
      | Module (venv, menv, cenv) -> env_extend false env (venv, menv, cenv)
      | Functor _ -> assert false
      | exception Not_found -> env
      (* Module might be a .mli only *)
    in
    eval_expr nenv e
  | Pexp_object _ -> assert false
  | Pexp_pack me -> ModVal (eval_module_expr env me)
  | Pexp_extension _ -> assert false

and eval_expr_exn env expr =
  try Ok (eval_expr env expr) with InternalException v -> Error v

and bind_value evalenv bindenv vb =
  let v = eval_expr evalenv vb.pvb_expr in
  pattern_bind bindenv vb.pvb_pat v

and bind_value_rec evalenvref bindenv vb =
  let v = eval_fun_or_function evalenvref vb.pvb_expr in
  pattern_bind bindenv vb.pvb_pat v

and pattern_bind env pat v =
  match pat.ppat_desc with
  | Ppat_any -> env
  | Ppat_var s -> env_set_value s.txt v env
  | Ppat_alias (p, s) -> env_set_value s.txt v (pattern_bind env p v)
  | Ppat_constant c ->
    if value_equal (value_of_constant c) v then env else raise Match_fail
  | Ppat_interval (c1, c2) ->
    if value_le (value_of_constant c1) v && value_le v (value_of_constant c2)
    then env
    else raise Match_fail
  | Ppat_tuple l ->
    (match v with
    | Tuple vl ->
      assert (List.length l = List.length vl);
      List.fold_left2 pattern_bind env l vl
    | _ -> assert false)
  | Ppat_construct (c, p) ->
    let cn = lident_name c.txt in
    let dn = env_get_constr env c in
    (match v with
    | Constructor (ccn, ddn, e) ->
      if cn <> ccn then raise Match_fail;
      if dn <> ddn then raise Match_fail;
      (match (p, e) with
      | None, None -> env
      | Some p, Some e -> pattern_bind env p e
      | _ -> assert false)
    | String s ->
      assert (lident_name c.txt = "Format");
      let p =
        match p with
        | None -> assert false
        | Some p -> p
      in
      let fmt_ebb_of_string =
        let lid =
          Longident.(Ldot (Lident "CamlinternalFormat", "fmt_ebb_of_string")) in
        env_get_value env { loc = c.loc; txt = lid } in
      let fmt = apply fmt_ebb_of_string [ (Nolabel, String s) ] in
      let fmt =
        match fmt with
        | Constructor ("Fmt_EBB", _, Some fmt) -> fmt
        | _ -> assert false
      in
      pattern_bind env p (Tuple [ fmt; v ])
    | _ ->
      Format.eprintf "cn = %s@.v = %a@." cn pp_print_value v;
      assert false)
  | Ppat_variant (name, p) ->
    (match v with
    | Constructor (cn, _, e) ->
      if cn <> name then raise Match_fail;
      (match (p, e) with
      | None, None -> env
      | Some p, Some e -> pattern_bind env p e
      | _ -> assert false)
    | _ -> assert false)
  | Ppat_record (rp, _) ->
    (match v with
    | Record r ->
      List.fold_left
        (fun env (lident, p) ->
          pattern_bind env p !(SMap.find (lident_name lident.txt) r))
        env
        rp
    | _ -> assert false)
  | Ppat_array _ -> assert false
  | Ppat_or (p1, p2) ->
    (try pattern_bind env p1 v with Match_fail -> pattern_bind env p2 v)
  | Ppat_constraint (p, _) -> pattern_bind env p v
  | Ppat_type _ -> assert false
  | Ppat_lazy _ -> assert false
  | Ppat_unpack name ->
    (match v with
    | ModVal m -> env_set_module name.txt m env
    | _ -> assert false)
  | Ppat_exception _ -> raise Match_fail
  | Ppat_extension _ -> assert false
  | Ppat_open _ -> assert false

and pattern_bind_exn env pat v =
  match pat.ppat_desc with
  | Ppat_exception p -> pattern_bind env p v
  | _ -> raise Match_fail

and pattern_bind_checkexn env pat v =
  match v with
  | Ok v -> pattern_bind env pat v
  | Error v -> pattern_bind_exn env pat v

and eval_match env cl arg =
  match cl with
  | [] ->
    (match arg with
    | Ok _ -> raise Match_fail
    | Error v -> raise (InternalException v))
  | c :: cl ->
    (match pattern_bind_checkexn env c.pc_lhs arg with
    | exception Match_fail -> eval_match env cl arg
    | nenv ->
      let guard_ok =
        match c.pc_guard with
        | None -> true
        | Some guard -> is_true (eval_expr nenv guard)
      in
      if guard_ok then eval_expr nenv c.pc_rhs else eval_match env cl arg)

and eval_module_expr env me =
  match me.pmod_desc with
  | Pmod_ident lident -> env_get_module env lident
  | Pmod_structure str -> make_module (eval_structure None env str)
  | Pmod_functor ({ txt = arg_name; _ }, _, e) -> Functor (arg_name, e, env)
  | Pmod_constraint (me, _) -> eval_module_expr env me
  | Pmod_apply (me1, me2) ->
    let m1 = eval_module_expr env me1 in
    let m2 = eval_module_expr env me2 in
    (match m1 with
    | Module _ -> assert false
    | Functor (arg_name, body, env) ->
      eval_module_expr (env_set_module arg_name m2 env) body)
  | Pmod_unpack e ->
    (match eval_expr env e with
    | ModVal m -> m
    | _ -> assert false)
  | Pmod_extension _ -> assert false

and eval_structitem init_ignored env it =
  match it.pstr_desc with
  | Pstr_eval (e, _) ->
    let v = eval_expr env e in
    Format.printf "%a@." pp_print_value v;
    env
  | Pstr_value (f, vals) ->
    if f = Nonrecursive
    then List.fold_left (bind_value env) env vals
    else (
      let er = ref env in
      let nenv = List.fold_left (bind_value_rec er) env vals in
      er := nenv;
      nenv)
  | Pstr_primitive { pval_name = { txt = name; loc }; pval_prim = l; _ } ->
    let prim_name = List.hd l in
    let prim =
      try SMap.find prim_name Primitives.prims
      with Not_found ->
        Prim
          (fun _ ->
            if debug then Format.eprintf "%a@." Location.print_loc loc;
            failwith ("Unimplemented primitive " ^ prim_name))
    in
    env_set_value name prim env
  | Pstr_type (_, tl) ->
    List.fold_left
      (fun env t ->
        match t.ptype_kind with
        | Ptype_variant l ->
          let _, _, env =
            List.fold_left
              (fun (u, v, env) cd ->
                match cd.pcd_args with
                | Pcstr_tuple [] ->
                  (u + 1, v, env_set_constr cd.pcd_name.txt u env)
                | _ -> (u, v + 1, env_set_constr cd.pcd_name.txt v env))
              (0, 0, env)
              l
          in
          env
        | _ -> env)
      env
      tl
  | Pstr_typext _ -> env
  | Pstr_exception { pext_name = name; pext_kind = k; _ } ->
    (match k with
    | Pext_decl _ ->
      let d = Primitives.next_exn_id () in
      env_set_constr name.txt d env
    | Pext_rebind path -> env_set_constr name.txt (env_get_constr env path) env)
  | Pstr_module { pmb_name = name; pmb_expr = me; _ } ->
    (match init_ignored with
    | None -> env_set_module name.txt (eval_module_expr env me) env
    | Some ign ->
      (try env_set_module name.txt (eval_module_expr env me) env
       with Not_found ->
         assert (
           match me.pmod_desc with
           | Pmod_ident { txt = Longident.Lident s; _ } -> s = name.txt
           | _ -> false);
         ign := SSet.add name.txt !ign;
         env))
  | Pstr_recmodule _ -> assert false
  | Pstr_modtype _ -> env
  | Pstr_open { popen_lid = lident; _ } ->
    (match env_get_module env lident with
    | Module (venv, menv, cenv) -> env_extend false env (venv, menv, cenv)
    | Functor _ -> assert false)
  | Pstr_class _ -> assert false
  | Pstr_class_type _ -> assert false
  | Pstr_include { pincl_mod = me; _ } ->
    let m = eval_module_expr env me in
    (match m with
    | Module (venv, menv, cenv) -> env_extend true env (venv, menv, cenv)
    | Functor _ -> assert false)
  | Pstr_attribute _ -> env
  | Pstr_extension _ -> assert false

and eval_structure_ init_ignored env str =
  match str with
  | [] -> env
  | it :: str ->
    eval_structure_ init_ignored (eval_structitem init_ignored env it) str

and eval_structure init_ignored env str =
  eval_structure_ init_ignored (prevent_export env) str

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
  let env = eval_structure (Some ign) !Primitives.initial_env stdlib_main in
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

