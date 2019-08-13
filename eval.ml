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
  | _ -> []

let rec apply prims vf args =
  let vf, extral, extram =
    match Ptr.get vf with
    | Fun_with_extra_args (vf, extral, extram) -> (vf, extral, extram)
    | _ -> (vf, [], SMap.empty)
  in
  assert (extral = []);
  (* let ls = fun_label_shape vf in *)
  let apply_labelled vf (lab, arg) =
    match Ptr.get vf with
    | Fun (label, default, p, e, fenv) ->
      (match (label, lab, default) with
      | Optional s, Labelled s', None ->
        assert (s = s');
        eval_expr
          prims
          (pattern_bind prims fenv p (ptr @@ Constructor ("Some", 0, Some arg)))
          e
      | Optional s, Labelled s', Some _
      | Optional s, Optional s', None
      | Labelled s, Labelled s', None ->
        assert (s = s');
        eval_expr prims (pattern_bind prims fenv p arg) e
      | Optional s, Optional s', Some def ->
        assert (s = s');
        let arg =
          match Ptr.get arg with
          | Constructor ("None", 0, None) -> eval_expr prims fenv def
          | Constructor ("Some", 0, Some arg) -> arg
          | _ -> assert false
        in
        eval_expr prims (pattern_bind prims fenv p arg) e
      | _ -> assert false)
    | _ -> assert false
  in
  let apply_optional_noarg vf =
    match Ptr.get vf with
    | Fun (Optional _, None, p, e, fenv) ->
      eval_expr
        prims
        (pattern_bind prims fenv p (ptr @@ Constructor ("None", 0, None)))
        e
    | Fun (Optional _, Some def, p, e, fenv) ->
      eval_expr
        prims
        (pattern_bind prims fenv p (eval_expr prims fenv def))
        e
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
    match Ptr.get vf with
    | Fun (Nolabel, _default, p, e, fenv) ->
      eval_expr prims (pattern_bind prims fenv p arg) e
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
      else eval_expr prims (pattern_bind prims fenv p arg) e
    | Function (cl, fenv) -> eval_match prims fenv cl (Ok arg)
    | Prim prim -> prim arg
    | _ ->
      Format.eprintf "%a@." pp_print_value vf;
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
        match Ptr.get vf with
        | Fun (((Labelled s | Optional s) as lab), _default, _p, _e, _fenv) ->
          if SMap.mem s !with_label
          then (
            let v = SMap.find s !with_label in
            with_label := SMap.remove s !with_label;
            apply_loop (apply_labelled vf v))
          else (
            assert (lab = Optional s);
            apply_loop (apply_optional_noarg vf))
        | _ -> ptr @@ Fun_with_extra_args (vf, [], !with_label))
    in
    apply_loop vf)

and eval_expr prims env expr =
  match expr.pexp_desc with
  | Pexp_ident id -> env_get_value env id
  | Pexp_constant c -> value_of_constant c
  | Pexp_let (recflag, vals, e) ->
    eval_expr prims (eval_bindings prims env recflag vals) e
  | Pexp_function cl -> ptr @@ Function (cl, env)
  | Pexp_fun (label, default, p, e) -> ptr @@ Fun (label, default, p, e, env)
  | Pexp_apply (f, l) ->
    (match Ptr.get @@ eval_expr prims env f with
    | Fexpr fexpr ->
      let loc = expr.pexp_loc in
      (match fexpr loc l with
      | None ->
        if debug
        then Format.eprintf "%a@.F-expr failure.@." Location.print_loc loc;
        assert false
      | Some expr -> eval_expr prims env expr)
    | func_value ->
      let args = List.map (fun (lab, e) -> (lab, eval_expr prims env e)) l in
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
      apply prims (ptr @@ func_value) args)
  | Pexp_tuple l ->
    let args = List.map (eval_expr prims env) l in
    ptr @@ Tuple args
  | Pexp_match (e, cl) -> eval_match prims env cl (eval_expr_exn prims env e)
  | Pexp_coerce (e, _, _) -> eval_expr prims env e
  | Pexp_constraint (e, _) -> eval_expr prims env e
  | Pexp_sequence (e1, e2) ->
    let _ = eval_expr prims env e1 in
    eval_expr prims env e2
  | Pexp_while (e1, e2) ->
    while is_true (eval_expr prims env e1) do
      ignore (eval_expr prims env e2)
    done;
    unit
  | Pexp_for (p, e1, e2, flag, e3) ->
    let v1 = Runtime_base.unwrap_int (eval_expr prims env e1) in
    let v2 = Runtime_base.unwrap_int (eval_expr prims env e2) in
    if flag = Upto
    then
      for x = v1 to v2 do
        let vx = Runtime_base.wrap_int x in
        ignore (eval_expr prims (pattern_bind prims env p vx) e3)
      done
    else
      for x = v1 downto v2 do
        let vx = Runtime_base.wrap_int x in
        ignore (eval_expr prims (pattern_bind prims env p vx) e3)
      done;
    unit
  | Pexp_ifthenelse (e1, e2, e3) ->
    if is_true (eval_expr prims env e1)
    then eval_expr prims env e2
    else (
      match e3 with
      | None -> unit
      | Some e3 -> eval_expr prims env e3)
  | Pexp_unreachable -> failwith "reached unreachable"
  | Pexp_try (e, cs) ->
    (try eval_expr prims env e
     with InternalException v ->
       (try eval_match prims env cs (Ok v)
        with Match_fail -> raise (InternalException v)))
  | Pexp_construct (c, e) ->
    let cn = lident_name c.txt in
    let d = env_get_constr env c in
    let ee =
      match e with
      | None -> None
      | Some e -> Some (eval_expr prims env e)
    in
    ptr @@ Constructor (cn, d, ee)
  | Pexp_variant (cn, e) ->
    let ee =
      match e with
      | None -> None
      | Some e -> Some (eval_expr prims env e)
    in
    ptr @@ Constructor (cn, Hashtbl.hash cn, ee)
  | Pexp_record (r, e) ->
    let base =
      match e with
      | None -> SMap.empty
      | Some e ->
        (match Ptr.get @@ eval_expr prims env e with
        | Record r -> r
        | _ -> assert false)
    in
    ptr @@ Record
      (List.fold_left
         (fun rc ({ txt = lident; _ }, ee) ->
           SMap.add (lident_name lident) (ref (eval_expr prims env ee)) rc)
         base
         r)
  | Pexp_field (e, { txt = lident; _ }) ->
    (match Ptr.get @@ eval_expr prims env e with
    | Record r -> !(SMap.find (lident_name lident) r)
    | _ -> assert false)
  | Pexp_setfield (e1, { txt = lident; _ }, e2) ->
    let v1 = eval_expr prims env e1 in
    let v2 = eval_expr prims env e2 in
    (match Ptr.get @@ v1 with
    | Record r ->
      SMap.find (lident_name lident) r := v2;
      unit
    | _ -> assert false)
  | Pexp_array l -> ptr @@ Array (Array.of_list (List.map (eval_expr prims env) l))
  | Pexp_send _ -> assert false
  | Pexp_new _ -> assert false
  | Pexp_setinstvar _ -> assert false
  | Pexp_override _ -> assert false
  | Pexp_letexception ({ pext_name = name; pext_kind = k; _ }, e) ->
    let nenv =
      match k with
      | Pext_decl _ ->
        let d = next_exn_id () in
        env_set_constr name.txt d env
      | Pext_rebind path ->
        env_set_constr name.txt (env_get_constr env path) env
    in
    eval_expr prims nenv e
  | Pexp_letmodule (name, me, e) ->
    let m = eval_module_expr prims env me in
    eval_expr prims (env_set_module name.txt m env) e
  | Pexp_assert e ->
    if is_true (eval_expr prims env e)
    then unit
    else (
      (*failwith "assert failure"*)
      let loc = expr.pexp_loc in
      let Lexing.{ pos_fname; pos_lnum; pos_cnum; _ } =
        loc.Location.loc_start
      in
      raise
        (InternalException
           (Runtime_base.assert_failure_exn pos_fname pos_lnum pos_cnum)))
  | Pexp_lazy e -> ptr @@ Lz (ref (fun () -> eval_expr prims env e))
  | Pexp_poly _ -> assert false
  | Pexp_newtype (_, e) -> eval_expr prims env e
  | Pexp_open (_, lident, e) ->
    let nenv =
      match env_get_module_data env lident with
      | exception Not_found ->
        (* Module might be a .mli only *)
        env
      | module_data -> env_extend false env module_data
    in
    eval_expr prims nenv e
  | Pexp_object _ -> assert false
  | Pexp_pack me -> ptr @@ ModVal (eval_module_expr prims env me)
  | Pexp_extension _ -> assert false

and eval_expr_exn prims env expr =
  try Ok (eval_expr prims env expr) with InternalException v -> Error v

and bind_value prims env vb =
  let v = eval_expr prims env vb.pvb_expr in
  pattern_bind prims env vb.pvb_pat v

and eval_bindings prims env recflag defs =
  match recflag with
    | Nonrecursive ->
       List.fold_left (bind_value prims) env defs
    | Recursive ->
       let dummies = List.map (fun _ -> Ptr.dummy ()) defs in
       let declare env vb dummy =
         pattern_bind prims env vb.pvb_pat dummy in
       let define env vb dummy =
         let v = eval_expr prims env vb.pvb_expr in
         Ptr.backpatch dummy (Ptr.get v) in
       let nenv = List.fold_left2 declare env defs dummies in
       List.iter2 (define nenv) defs dummies;
       nenv

and pattern_bind prims env pat v =
  match pat.ppat_desc with
  | Ppat_any -> env
  | Ppat_var s -> env_set_value s.txt v env
  | Ppat_alias (p, s) -> env_set_value s.txt v (pattern_bind prims env p v)
  | Ppat_constant c ->
    if value_equal (value_of_constant c) v then env else raise Match_fail
  | Ppat_interval (c1, c2) ->
    if value_le (value_of_constant c1) v && value_le v (value_of_constant c2)
    then env
    else raise Match_fail
  | Ppat_tuple l ->
    (match Ptr.get v with
    | Tuple vl ->
      assert (List.length l = List.length vl);
      List.fold_left2 (pattern_bind prims) env l vl
    | _ -> assert false)
  | Ppat_construct (c, p) ->
    let cn = lident_name c.txt in
    let dn = env_get_constr env c in
    (match Ptr.get v with
    | Constructor (ccn, ddn, e) ->
      if cn <> ccn then raise Match_fail;
      if dn <> ddn then raise Match_fail;
      (match (p, e) with
      | None, None -> env
      | Some p, Some e -> pattern_bind prims env p e
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
          Longident.(Ldot (Lident "CamlinternalFormat", "fmt_ebb_of_string"))
        in
        env_get_value env { loc = c.loc; txt = lid }
      in
      let fmt = apply prims fmt_ebb_of_string [ (Nolabel, ptr @@ String s) ] in
      let fmt =
        match Ptr.get fmt with
        | Constructor ("Fmt_EBB", _, Some fmt) -> fmt
        | _ -> assert false
      in
      pattern_bind prims env p (ptr @@ Tuple [ fmt; v ])
    | _ ->
      Format.eprintf "cn = %s@.v = %a@." cn pp_print_value v;
      assert false)
  | Ppat_variant (name, p) ->
    (match Ptr.get v with
    | Constructor (cn, _, e) ->
      if cn <> name then raise Match_fail;
      (match (p, e) with
      | None, None -> env
      | Some p, Some e -> pattern_bind prims env p e
      | _ -> assert false)
    | _ -> assert false)
  | Ppat_record (rp, _) ->
    (match Ptr.get v with
    | Record r ->
      List.fold_left
        (fun env (lident, p) ->
          pattern_bind prims env p !(SMap.find (lident_name lident.txt) r))
        env
        rp
    | _ -> assert false)
  | Ppat_array _ -> assert false
  | Ppat_or (p1, p2) ->
    (try pattern_bind prims env p1 v
     with Match_fail -> pattern_bind prims env p2 v)
  | Ppat_constraint (p, _) -> pattern_bind prims env p v
  | Ppat_type _ -> assert false
  | Ppat_lazy _ -> assert false
  | Ppat_unpack name ->
    (match Ptr.get v with
    | ModVal m -> env_set_module name.txt m env
    | _ -> assert false)
  | Ppat_exception _ -> raise Match_fail
  | Ppat_extension _ -> assert false
  | Ppat_open _ -> assert false

and pattern_bind_exn prims env pat v =
  match pat.ppat_desc with
  | Ppat_exception p -> pattern_bind prims env p v
  | _ -> raise Match_fail

and pattern_bind_checkexn prims env pat v =
  match v with
  | Ok v -> pattern_bind prims env pat v
  | Error v -> pattern_bind_exn prims env pat v

and eval_match prims env cl arg =
  match cl with
  | [] ->
    (match arg with
    | Ok _ -> raise Match_fail
    | Error v -> raise (InternalException v))
  | c :: cl ->
    (match pattern_bind_checkexn prims env c.pc_lhs arg with
    | exception Match_fail -> eval_match prims env cl arg
    | nenv ->
      let guard_ok =
        match c.pc_guard with
        | None -> true
        | Some guard -> is_true (eval_expr prims nenv guard)
      in
      if guard_ok
      then eval_expr prims nenv c.pc_rhs
      else eval_match prims env cl arg)

and eval_module_expr prims env me =
  match me.pmod_desc with
  | Pmod_ident lident -> env_get_module env lident
  | Pmod_structure str -> Module (make_module_data (eval_structure prims env str))
  | Pmod_functor ({ txt = arg_name; _ }, _, e) -> Functor (arg_name, e, env)
  | Pmod_constraint (me, _) -> eval_module_expr prims env me
  | Pmod_apply (me1, me2) ->
    let m1 = eval_module_expr prims env me1 in
    let m2 = eval_module_expr prims env me2 in
    let arg_name, body, env = eval_functor_data env me.pmod_loc m1 in
    eval_module_expr prims (env_set_module arg_name m2 env) body
  | Pmod_unpack e ->
    (match Ptr.get @@ eval_expr prims env e with
    | ModVal m -> m
    | _ -> assert false)
  | Pmod_extension _ -> assert false

and eval_functor_data env loc = function
  | Module _ -> failwith "tried to apply a simple module"
  | Unit _ -> failwith "tried to apply a simple module unit"
  | Functor (arg_name, body, env) -> (arg_name, body, env)

and eval_structitem prims env it =
  match it.pstr_desc with
  | Pstr_eval (e, _) ->
    let v = eval_expr prims env e in
    Format.printf "%a@." pp_print_value v;
    env
  | Pstr_value (recflag, defs) -> eval_bindings prims env recflag defs
  | Pstr_primitive { pval_name = { txt = name; loc }; pval_prim = l; _ } ->
    let prim_name = List.hd l in
    let prim =
      try SMap.find prim_name prims
      with Not_found ->
        ptr @@ Prim
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
      let d = next_exn_id () in
      env_set_constr name.txt d env
    | Pext_rebind path -> env_set_constr name.txt (env_get_constr env path) env)
  | Pstr_module { pmb_name = name; pmb_expr = me; _ } ->
     env_set_module name.txt (eval_module_expr prims env me) env
  | Pstr_recmodule _ -> assert false
  | Pstr_modtype _ -> env
  | Pstr_open { popen_lid = lident; _ } ->
    env_extend false env (env_get_module_data env lident)
  | Pstr_class _ -> assert false
  | Pstr_class_type _ -> assert false
  | Pstr_include { pincl_mod = me; pincl_loc = loc; _ } ->
    let m = eval_module_expr prims env me in
    env_extend true env (get_module_data env loc m)
  | Pstr_attribute _ -> env
  | Pstr_extension _ -> assert false

and eval_structure_ prims env str =
  match str with
  | [] -> env
  | it :: str ->
    eval_structure_
      prims
      (eval_structitem prims env it)
      str

and eval_structure prims env str =
  eval_structure_ prims (prevent_export env) str
