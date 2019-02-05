open Ast

module SMap = Map.Make(String)

type print_elem =
  | Line of string
  | IndentChange of int

type 'a catenable_list =
  | Atom of 'a list
  | Cat of 'a catenable_list list

let rec flatten_catenable_list l =
  let rec aux l r = match l with
    | Atom l -> l @ r
    | Cat l -> List.fold_right aux l r
  in
  aux l []

let cur_tempvars = ref 0
let mktempvar tvindex =
  cur_tempvars := max (tvindex + 1) !cur_tempvars;
  "tmp__" ^ string_of_int tvindex

let list_max = List.fold_left max 0

let pattern_vars = function
  | PVar v -> if v = "_" then 0 else 1
  | PConstructor (_, l) -> List.fold_left (+) 0 (List.map (fun v -> if v = "_" then 0 else 1) l)
  | PInt _ -> 0

let pp_sep_string x ff () = Format.fprintf ff "%s" x
let pp_sep_comma = pp_sep_string ", "

let pp_print_label ff = function
  | Nolabel -> ()
  | Labelled s -> Format.fprintf ff "~%s:" s
  | Optional s -> Format.fprintf ff "?%s:" s

let rec print_tempvars tv =
  if tv = 0 then
    Atom []
  else
    Cat [
      print_tempvars (tv - 1);
      Atom [Line ("CAMLlocal1(" ^ mktempvar (tv - 1) ^ ");")]
    ]

type env = {
  env_name_prefix : string ;
  (* second boolean to say whether the variable is a local variable of the function or a global variable *)
  env_vars : (bool * (bool * string)) SMap.t ;
  (* int = arity *)
  env_constrs : (bool * (int * string)) SMap.t ;
  env_fields : (bool * string) SMap.t ;
  env_modules : (bool * env) SMap.t ;
}

let rec env_get_module env = function
  | Lident s -> snd (SMap.find s env.env_modules)
  | Ldot (l, s) -> snd (SMap.find s (env_get_module env l).env_modules)

let rec longident_name = function
  | Lident s -> s
  | Ldot (l, s) -> longident_name l ^ "__" ^ s

let env_get_env_li env = function
  | Lident s -> env, s
  | Ldot (l, s) -> env_get_module env l, s

let env_get_var env li =
  try let env, s = env_get_env_li env li in snd (SMap.find s env.env_vars)
  with Not_found -> (false, longident_name li)

let env_get_constr env li =
  try let env, s = env_get_env_li env li in snd (SMap.find s env.env_constrs)
  with Not_found -> (-1, "tag__" ^ longident_name li)

let env_get_field env li =
  try let env, s = env_get_env_li env li in snd (SMap.find s env.env_fields)
  with Not_found -> "field__" ^ longident_name li

let env_add_tempvar env name tvindex =
  { env with env_vars = SMap.add name (true, (true, mktempvar tvindex)) env.env_vars }

let env_remove_tempvars env =
  { env with env_vars = SMap.mapi (fun name ((export, (is_temp, _)) as v) ->
        if is_temp then (export, (true, "ERROR_VAR_OF_CLOSURE_" ^ name)) else v) env.env_vars }

let rec range a b = if a >= b then [] else a :: range (a + 1) b

let lref = ref 0
let gen_label () =
  incr lref; "label" ^ string_of_int !lref
let gen_lambda () =
  incr lref; "lambda" ^ string_of_int !lref

let rec split_pattern_matching env = function
  | [] -> [], [], None
  | (PVar v, e) :: _ -> [], [], Some (v, e)
  | (PConstructor (c, l), e) :: r ->
    let a, b, v = split_pattern_matching env r in
    let arity, cc = env_get_constr env c in
    if l = [] then begin
      assert (arity = 0 || arity = -1);
      (cc, e) :: a, b, v
    end else begin
      assert (arity <> 0);
      a, (cc, arity, l, e) :: b, v
    end
  | (PInt n, e) :: r ->
    let a, b, v = split_pattern_matching env r in
    (n, e) :: a, b, v


let init_env = {
  env_name_prefix = "" ;
  env_vars = SMap.singleton "assert" (false, (false, "caml_assert")) ;
  env_constrs = SMap.empty ;
  env_fields = SMap.empty ;
  env_modules = SMap.empty ;
}

let fun_shapes = ref SMap.empty
let decls = ref []
let defs = ref []
let inits = ref []
let init_tempvars = ref 0

let rec print_elems ff indent elems =
  match elems with
  | [] -> ()
  | Line s :: elems ->
    Format.fprintf ff "%s" (String.make indent ' ');
    Format.fprintf ff "%s@." s;
    print_elems ff indent elems
  | IndentChange c :: elems -> print_elems ff (indent + c) elems

let escaped s =
  let res = Bytes.make (4 * String.length s) '\\' in
  let hex = "0123456789abcdef" in
  for i = 0 to String.length s - 1 do
    res.[4 * i] <- '\\';
    res.[4 * i + 1] <- 'x';
    res.[4 * i + 2] <- hex.[(Char.code s.[i]) lsr 4];
    res.[4 * i + 3] <- hex.[(Char.code s.[i]) land 15];
  done;
  Bytes.to_string res

let rec print_expr env tvindex ret err is_tail expr =
  let (rf1, rf2) = ret in
  let result x = Line (rf1 ^ "(" ^ x ^ ")" ^ rf2) in
  match expr with
  | EVar v ->
    (* Convert to a value in case the variable was referring to a function *)
    Atom [result ("(value)" ^ snd (env_get_var env v))]
  | EConstant (CInt i) ->
    Atom [result ("Val_long(" ^ i ^ ")")]
  | EConstant CUnit ->
    Atom [result "Val_unit"]
  | EConstant (CString s) ->
    Atom [result ("caml_alloc_initialized_string(" ^ string_of_int (String.length s) ^ ", \"" ^ escaped s ^ "\")")]
  | EConstr (name, []) ->
    let arity, c = env_get_constr env name in
    assert (arity = 0 || arity = -1);
    Atom [result ("Val_long(" ^ c ^ ")")]
  | EConstr (name, args) ->
    let tempvar = mktempvar tvindex in
    let arity, c = env_get_constr env name in
    let args = match args with [EConstr (Lident "", args)] when arity <> 1 -> args | _ -> args in
    assert (arity <> 0);
    assert (arity = -1 || arity = 1 || arity = List.length args);
    Cat [
      Atom [Line (tempvar ^ " = caml_alloc(" ^ string_of_int (List.length args) ^ ", " ^ c ^ ");")];
      Cat (List.mapi (fun i e ->
          print_expr env (tvindex + 1) ("Store_field(" ^ tempvar ^ ", " ^ string_of_int i ^ ", ", ");") err false e
      ) args);
      Atom [result tempvar]
    ]
  | EGetfield (e, f) ->
    Cat [
      print_expr env tvindex ("tmp = ", ";") err false e;
      Atom [result ("Field(tmp, " ^ env_get_field env f ^ ")")]
    ]
  | ESetfield (e1, f, e2) ->
    let tempvar = mktempvar tvindex in
    Cat [
      print_expr env tvindex (tempvar ^ " = ", ";") err false e1;
      print_expr env (tvindex + 1) ("Store_field(" ^ tempvar ^ ", " ^ env_get_field env f ^ ", ", ");") err false e2;
      Atom [result "Val_unit"]
    ]
  | ERecord args ->
    assert (List.length args > 0);
    let tempvar = mktempvar tvindex in
    Cat [
      Atom [Line (tempvar ^ " = caml_alloc(" ^ string_of_int (List.length args) ^ ", 0);")];
      Cat (List.map (fun (f, e) ->
          print_expr env (tvindex + 1) ("Store_field(" ^ tempvar ^ ", " ^ env_get_field env f ^ ", ", ");") err false e
      ) args);
      Atom [result tempvar]
    ]
  | ERecordwith (e, args) ->
    let tempvar = mktempvar tvindex in
    Cat [
      print_expr env tvindex (tempvar ^ " = ", ";") err false e;
      Atom [
        Line ("tmp = caml_alloc(Wosize_val(" ^ tempvar ^ "), 0);");
        Line ("for (size_t i = 0; i < Wosize_val(" ^ tempvar ^ "); i++) { Store_field(tmp, i, Field(" ^ tempvar ^ ", i)); }; " ^ tempvar ^ " = tmp;");
      ];
      Cat (List.map (fun (f, e) ->
          print_expr env (tvindex + 1) ("Store_field(" ^ tempvar ^ ", " ^ env_get_field env f ^ ", ", ");") err false e
      ) args);
      Atom [result tempvar]
    ]
  | EApply (f, args) ->
    let is_tempvar, f = env_get_var env f in
    let funshape =
      if is_tempvar then
        List.map (fun _ -> Nolabel) args
      else
        try
          SMap.find f !fun_shapes
        with Not_found ->
          List.map (fun _ -> Nolabel) args
    in
    let has_labelled_arg = List.exists (fun (_, lab) -> lab <> Nolabel) args in
    let rec extract_first f l =
      match l with
      | [] -> raise Not_found
      | x :: l -> if f x then x, l else let a, l1 = extract_first f l in a, x :: l1
    in
    (* Format.eprintf "%s@." f; *)
    let rec align shape args =
      match shape with
      | [] -> assert (args = []); []
      | Nolabel :: shape ->
        let (e, _), nargs = extract_first (fun (_, lab) -> lab = Nolabel) args in
        e :: align shape nargs
      | Labelled s :: shape ->
        let (e, _), nargs =
          if has_labelled_arg then
            extract_first (fun (_, lab) -> lab = Labelled s) args
          else
            extract_first (fun (_, lab) -> lab = Nolabel) args
        in
        e :: align shape nargs
      | Optional s :: shape ->
        let (e, lab), nargs =
          try extract_first (fun (_, lab) -> lab = Labelled s || lab = Optional s) args
          with Not_found -> ((EConstr (Lident "None", []), Optional s), args)
        in
        let e = match lab with Labelled _ -> EConstr (Lident "Some", [e]) | _ -> e in
        e :: align shape nargs
    in
    let args = align funshape args in
    let callf =
      if is_tempvar then
        "((value (*)(" ^ String.concat ", " (List.map (fun _ -> "value") args) ^ "))" ^ f ^ ")"
      else
        f
    in
    let callexpr =
      callf ^ "(" ^
      String.concat ", " (List.map mktempvar (range tvindex (tvindex + List.length args))) ^ ")"
    in
    Cat [
      Cat (List.mapi (fun i e ->
          print_expr env (tvindex + i) (mktempvar (tvindex + i) ^ " = ", ";") err false e
      ) args);
      if is_tail then
        Atom [result callexpr]
      else
        Atom [
          Line ("tmp = (" ^ callexpr ^ ");");
          Line ("if (Is_block(tmp) && Tag_val(tmp) == exn_tag) { " ^ err ^ " }");
          result "tmp";
        ]
    ]
  | EIf (e1, e2, e3) ->
    Cat [
      print_expr env tvindex ("tmp = ", ";") err false e1;
      Atom [Line "if (tmp != Val_false) {"; IndentChange 2];
      print_expr env tvindex ret err is_tail e2;
      Atom [IndentChange (-2); Line "} else {"; IndentChange 2];
      print_expr env tvindex ret err is_tail e3;
      Atom [IndentChange (-2); Line "}"]
    ]
  | EChain (e1, e2) ->
    Cat [
      print_expr env tvindex ("tmp = ", ";") err false e1;
      print_expr env tvindex ret err is_tail e2
    ]
  | EMatch (e, l) ->
    Cat [
      print_expr env tvindex ("tmp = ", ";") err false e;
      print_match env tvindex ret err is_tail false l
    ]
  | ELet (bindings, body) ->
    (* HACK: sequential let! *)
    (match bindings with
     | [] -> print_expr env tvindex ret err is_tail body
     | (p, e) :: rest ->
       print_expr env tvindex ret err is_tail (EMatch (e, [(p, ELet (rest, body))])))
  | ELambda (args, body) ->
    let fullname = gen_lambda () in
    let ct = !cur_tempvars in
    cur_tempvars := 0;
    process_fundef (env_remove_tempvars env) fullname (List.map (fun x -> (x, Nolabel, None)) args) body;
    cur_tempvars := ct;
    Atom [result ("(value)(&" ^ fullname ^ ")")]
  | ETry (e, m) ->
    let lab = gen_label () in
    Cat [
      print_expr env tvindex ret ("goto " ^ lab ^ ";") false e;
      Atom [Line ("if (0) { " ^ lab ^ ":"); IndentChange 2; Line "tmp = Field(tmp, 0);"];
      print_match env tvindex ret err is_tail true m;
      Atom [IndentChange (-2); Line "}"];
    ]

and print_match env tvindex ret err is_tail is_exn_matching l =
  let no_arg, with_arg, default = split_pattern_matching env l in
  let rf1, rf2 = ret in
  let result x = Line (rf1 ^ "(" ^ x ^ ")" ^ rf2) in
  assert (l <> []);
  let print_default =
    match default with
    | None -> if is_exn_matching then Atom [result "_raise(tmp)"] else Atom []
    | Some (vn, e) ->
      if vn = "_" then
        print_expr env tvindex ret err is_tail e
      else
        Cat [
          Atom [Line (mktempvar tvindex ^ " = tmp;")];
          print_expr (env_add_tempvar env vn tvindex) (tvindex + 1) ret err is_tail e
        ]
  in
  if (no_arg = [] && with_arg = []) then begin
    print_default
  end else begin
    let has_def = default <> None in
    let lab_def = if has_def || is_exn_matching then gen_label () else "BUG" in
    let do_def = if has_def then "goto " ^ lab_def ^ ";" else "assert(0);" in
    Cat [
      Atom [Line ("if (Is_long(tmp)) { switch (Int_val(tmp)) {"); IndentChange 2];
      Cat (List.map (fun (c, e) ->
          Cat [
            Atom [Line ("case " ^ c ^ ":"); IndentChange 2];
            print_expr env tvindex ret err is_tail e;
            Atom [Line "break;"; IndentChange (-2)]
          ]) no_arg);
      Atom [
        Line ("default: " ^ do_def);
        IndentChange (-2);
        Line "}} else { switch (Tag_val(tmp)) {";
        IndentChange 2
      ];
      Cat (List.map (fun (c, arity, l, e) ->
          let rec set_variables tvindex env i setvar = function
            | [] -> setvar, tvindex, env
            | "_" :: l -> set_variables tvindex env (i + 1) setvar l
            | x :: l ->
              let nsetvar =
                Cat [
                  setvar;
                  Atom [Line (mktempvar tvindex ^ " = Field(tmp, " ^ string_of_int i ^ ");")]
                ]
              in
              set_variables (tvindex + 1) (env_add_tempvar env x tvindex) (i + 1) nsetvar l
          in
          (* Format.eprintf "%s %d %d@." c arity (List.length l); *)
          assert (arity = -1 || arity = 1 || arity = List.length l || l = ["_"]);
          let setvar, ntv, nenv = set_variables tvindex env 0 (Atom []) l in
          Cat [
            Atom [Line ("case " ^ c ^ ":"); IndentChange 2];
            if arity = 1 && List.length l <> 1 then Atom [Line "tmp = Field(tmp, 0);"] else Atom [];
            setvar;
            print_expr nenv ntv ret err is_tail e;
            Atom [Line "break;"; IndentChange (-2)]
          ]) with_arg);
      Atom [
        Line ("default: " ^ do_def);
        IndentChange (-2);
        Line "}}"
      ];
      if has_def || is_exn_matching then
        Cat [
          Atom [Line ("if (0) { " ^ lab_def ^ ":"); IndentChange 2];
          print_default;
          Atom [IndentChange (-2); Line "}"]
        ]
      else
        Atom []
    ]
  end

and process_fundef tenv fullname args body =
  let ctv = !cur_tempvars in
  cur_tempvars := 0;
  let tenv = List.fold_left (fun env (x, _, _) ->
      { env with env_vars = SMap.add x (true, (true, x)) env.env_vars }) tenv args
  in
  let body = List.fold_right (fun (x, _, def) body ->
      match def with
      | None -> body
      | Some def ->
        ELet ([PVar x, EMatch (EVar (Lident x),
                               [(PConstructor (Lident "None", []), def);
                                (PConstructor (Lident "Some", [x]), EVar (Lident x))])], body)
    ) args body
  in
  let declaration =
    if args = [] then
      "value " ^ fullname
    else
      "value " ^ fullname ^ "(" ^ String.concat ", " (List.map (fun (x, _, _) -> "value " ^ x) args) ^ ")"
  in
  if fullname <> "_" then
    decls := Atom [Line (declaration ^ ";")] :: !decls;
  if args = [] then begin
    let initcode =
      if fullname = "_" then
        print_expr tenv 0 ("tmp = ", ";") "assert(0);" false body
      else
        Cat [
          print_expr tenv 0 (fullname ^ " = ", ";") "assert(0);" false body;
          Atom [Line ("caml_register_global_root(&" ^ fullname ^ ");")]
        ]
    in
    inits := initcode :: !inits;
    init_tempvars := max !init_tempvars !cur_tempvars
  end else begin
    let cbody = print_expr tenv 0 ("CAMLdrop; return ", ";") "CAMLdrop; return tmp;" true body in
    let fundef = Cat [
        Atom [
          Line (declaration ^ " {");
          IndentChange 2;
          Line ("CAMLparam" ^ string_of_int (List.length args) ^ "(" ^
                String.concat ", " (List.map (fun (x, _, _) -> x) args) ^ ");");
          Line "CAMLlocal1(tmp);"
        ];
        print_tempvars !cur_tempvars;
        cbody;
        Atom [IndentChange (-2); Line "}"; Line ""]
      ]
    in
    let labels = List.map (fun (_, lab, _) -> lab) args in
    fun_shapes := SMap.add fullname labels !fun_shapes;
    defs := fundef :: !defs;
  end;
  cur_tempvars := ctv

let exnid = ref 0

let rec process_def env = function
  | MOpen m ->
    (try
      let menv = env_get_module env m in
      { env with
        env_vars = SMap.fold (fun x (b, v) m -> if b then SMap.add x (false, v) m else m)
            menv.env_vars env.env_vars ;
        env_constrs = SMap.fold (fun x (b, v) m -> if b then SMap.add x (false, v) m else m)
            menv.env_constrs env.env_constrs ;
        env_fields = SMap.fold (fun x (b, v) m -> if b then SMap.add x (false, v) m else m)
            menv.env_fields env.env_fields ;
        env_modules = SMap.fold (fun x (b, v) m -> if b then SMap.add x (false, v) m else m)
            menv.env_modules env.env_modules ;
      }
     with Not_found -> env)
  | MException (name, arity) ->
    let fullname = "tag__" ^ env.env_name_prefix ^ name in
    let value = string_of_int !exnid in
    incr exnid;
    decls := Atom [Line ("#define " ^ fullname ^ " " ^ value)] :: !decls;
    { env with
      env_constrs = SMap.add name (true, (arity, fullname)) env.env_constrs
    }
  | MLet (rec_flag, l) ->
    let nenv =
      { env with
        env_vars = List.fold_left (fun m (name, _, _) ->
            if name = "_" then m else SMap.add name (true, (false, env.env_name_prefix ^ name)) m) env.env_vars l
      }
    in
    let tenv = if rec_flag then nenv else env in
    List.iter (fun (name, args, body) ->
        let fullname = if name = "_" then "_" else env.env_name_prefix ^ name in
        process_fundef tenv fullname args body
    ) l;
    nenv
  | MTypedef ts ->
    List.fold_left (fun env tr ->
        match tr with
        | (name, ISum l) ->
          let c1 = ref 0 in
          let c2 = ref 0 in
          let nenv = List.fold_left (fun env (n, arity) ->
              let c = if arity > 0 then c1 else c2 in
              let a = !c in incr c;
              let fullname = "tag__" ^ env.env_name_prefix ^ n in
              decls := Atom [Line ("#define " ^ fullname ^ " " ^ string_of_int a)] :: !decls;
              { env with env_constrs = SMap.add n (true, (arity, fullname)) env.env_constrs }
            ) env l
          in
          decls := Atom [Line ""] :: !decls;
          nenv
        | (name, IRecord l) ->
          let nenv, _ = List.fold_left (fun (env, i) n ->
              let fullname = "field__" ^ env.env_name_prefix ^ n in
              decls := Atom [Line ("#define " ^ fullname ^ " " ^ string_of_int i)] :: !decls;
              ({ env with env_fields = SMap.add n (true, fullname) env.env_fields }, i + 1)
            ) (env, 0) l
          in
          decls := Atom [Line ""] :: !decls;
          nenv
        | (_, IRebind) -> env
      ) env ts
  | MStruct (n, l) ->
    let modenv = {
      env_name_prefix = env.env_name_prefix ^ n ^ "__" ;
      env_vars = SMap.map (fun (_, v) -> (false, v)) env.env_vars ;
      env_constrs = SMap.map (fun (_, v) -> (false, v)) env.env_constrs ;
      env_fields = SMap.map (fun (_, v) -> (false, v)) env.env_fields ;
      env_modules = SMap.map (fun (_, v) -> (false, v)) env.env_modules ;
    } in
    let nenv = process_defs modenv l in
    { env with env_modules = SMap.add n (true, nenv) env.env_modules }
  | MExternal (n, s) ->
    let fullname = env.env_name_prefix ^ n in
    decls := Atom [Line ("#define " ^ fullname ^ " " ^ s)] :: !decls;
    { env with env_vars = SMap.add n (true, (false, fullname)) env.env_vars }

and process_defs env = function
  | [] -> env
  | d :: defs -> process_defs (process_def env d) defs

let header = [
  "#define CAML_NAME_SPACE";
  "#define CAML_INTERNALS";
  "#include <caml/alloc.h>";
  "#include <caml/mlvalues.h>";
  "#include <caml/memory.h>";
  "#include <caml/startup_aux.h>";
  "#include <stdio.h>";
  "#include <assert.h>";
  "#include <prims.c>";
  "#include \"std.h\"";
  "";
  "#define tag__Null 0";
  "#define tag__Cons 0";
  "";
  "#define tag__None 0";
  "#define tag__Some 0";
  "";
  "#define tag__ 0";
  "";
  "#define exn_tag 239";
  "";
  "value _raise(value x) { CAMLparam1(x); CAMLlocal1(res); res = caml_alloc(1, exn_tag); Store_field(res, 0, x); CAMLreturn(res); }";
]

let compile_and_print ff dfs =
  let _ = process_defs init_env dfs in
  let program = Cat [
      Atom (List.map (fun x -> Line x) header);
      Cat (List.rev !decls);
      Cat (List.rev !defs);
      Atom [Line "void init() {"; IndentChange 2; Line "CAMLparam0();"; Line "CAMLlocal1(tmp);"];
      print_tempvars !init_tempvars;
      Cat (List.rev !inits);
      Atom [Line "CAMLreturn0;"; IndentChange (-2); Line "}"];
      Atom [Line ""; Line "#include \"main.c\""];
    ]
  in
  print_elems ff 0 (flatten_catenable_list program);

