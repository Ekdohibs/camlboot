open Ast

module SMap = Map.Make(String)

let list_max = List.fold_left max 0

let pattern_vars = function
  | PVar v -> if v = "_" then 0 else 1
  | PConstructor (_, l) -> List.fold_left (+) 0 (List.map (fun v -> if v = "_" then 0 else 1) l)

let rec expr_tempvars = function
  | EVar _ -> 0
  | EConstant _ -> 0
  | EConstr (_, l) -> if l = [] then 0 else 1 + list_max (List.map expr_tempvars l)
  | EGetfield (e, _) -> expr_tempvars e
  | ERecord l -> 1 + list_max (List.map (fun (_, e) -> expr_tempvars e) l)
  | ERecordwith (e, l) ->
     max (expr_tempvars e) (1 + list_max (List.map (fun (_, e) -> expr_tempvars e) l))
  | EApply (_, l) ->
     max (List.length l) (list_max (List.mapi (fun i e -> i + expr_tempvars e) l))
  | EIf (e1, e2, e3) -> max (expr_tempvars e1) (max (expr_tempvars e2) (expr_tempvars e3))
  | EChain (e1, e2) -> max (expr_tempvars e1) (expr_tempvars e2)
  | EMatch (e, l) ->
      max (expr_tempvars e) (list_max (List.map (fun (p, e) -> pattern_vars p + expr_tempvars e) l))
  | ELet (p, e1, e2) ->
      max (expr_tempvars e1) (pattern_vars p + expr_tempvars e2)

let pp_sep_string x ff () = Format.fprintf ff "%s" x
let pp_sep_comma = pp_sep_string ", "

let print_decl ff = function
  | MLet (name, args, _) ->
     if args = [] then
       Format.fprintf ff "value %s;@." name
     else
       Format.fprintf ff "value %s(%a);@." name
        (Format.pp_print_list
           ~pp_sep:pp_sep_comma
           (fun ff x -> Format.fprintf ff "value %s" x)) args
  | MTypedef (name, ISum l) ->
     let c1 = ref 0 in
     let c2 = ref 0 in
     List.iter (fun (n, b) ->
       let c = if b then c1 else c2 in
       Format.fprintf ff "#define tag__%s %d@." n !c; incr c) l;
     Format.fprintf ff "@."
   | MTypedef (name, IRecord l) ->
     List.iteri (fun i n -> Format.fprintf ff "#define field__%s %d@." n i) l;
     Format.fprintf ff "@."

let rec print_tempvars ff tv =
  if tv > 0 then begin
    print_tempvars ff (tv - 1);
    Format.fprintf ff "CAMLlocal1(tmp__%d);" (tv - 1)
  end

let env_get env v =
  try "tmp__" ^ (string_of_int (SMap.find v env)) with Not_found -> v

let rec range a b = if a >= b then [] else a :: range (a + 1) b

let lref = ref 0
let gen_label () =
  incr lref; "label" ^ string_of_int !lref

let rec split_pattern_matching = function
  | [] -> [], [], None
  | (PVar v, e) :: _ -> [], [], Some (v, e)
  | (PConstructor (c, l), e) :: r ->
    let a, b, v = split_pattern_matching r in
    if l = [] then (c, e) :: a, b, v else a, (c, l, e) :: b, v


let fun_env = SMap.singleton "assert" "caml_assert"
let get_fun f = try SMap.find f fun_env with Not_found -> f

let rec print_expr ff env tvindex rf1 rf2 = function
  | EVar v -> Format.fprintf ff "%t%s%t" rf1 (env_get env v) rf2
  | EConstant _ -> assert false
  | EConstr (name, []) -> Format.fprintf ff "%t(Val_long(tag__%s))%t" rf1 name rf2
  | EConstr (name, args) ->
    Format.fprintf ff "tmp__%d = caml_alloc(%d, tag__%s);@," tvindex (List.length args) name;
    List.iteri (fun i e ->
      print_expr ff env (tvindex + 1)
        (fun ff -> Format.fprintf ff "Store_field(tmp__%d, %d, " tvindex i)
        (fun ff -> Format.fprintf ff ");@,")
      e) args;
    Format.fprintf ff "%ttmp__%d%t" rf1 tvindex rf2
  | EGetfield (e, f) ->
    print_expr ff env tvindex (fun ff -> Format.fprintf ff "tmp =") (fun ff -> Format.fprintf ff ";@,") e;
    Format.fprintf ff "%tField(tmp, field__%s)%t" rf1 f rf2
  | ERecord args ->
    assert (List.length args > 0);
    Format.fprintf ff "tmp__%d = caml_alloc(%d, 0);@," tvindex (List.length args);
    List.iter (fun (f, e) ->
      print_expr ff env (tvindex + 1)
        (fun ff -> Format.fprintf ff "Store_field(tmp__%d, field__%s, " tvindex f)
        (fun ff -> Format.fprintf ff ");@,")
      e) args;
    Format.fprintf ff "%ttmp__%d%t" rf1 tvindex rf2
  | ERecordwith (e, args) ->
    print_expr ff env tvindex (fun ff -> Format.fprintf ff "tmp__%d =" tvindex) (fun ff -> Format.fprintf ff ";@,") e;
    Format.fprintf ff "tmp = caml_alloc(Wosize_val(tmp__%d), 0);@," tvindex;
    Format.fprintf ff "for (size_t i = 0; i < Wosize_val(tmp__%d); i++) { Store_field(tmp, i, Field(tmp__%d, i)); }; tmp__%d = tmp;@," tvindex tvindex tvindex;
    List.iter (fun (f, e) ->
      print_expr ff env (tvindex + 1)
        (fun ff -> Format.fprintf ff "Store_field(tmp__%d, field__%s, " tvindex f)
        (fun ff -> Format.fprintf ff ");@,")
      e) args;
    Format.fprintf ff "%ttmp__%d%t" rf1 tvindex rf2
  | EApply (f, args) ->
    List.iteri (fun i e ->
      print_expr ff env (tvindex + i)
        (fun ff -> Format.fprintf ff "tmp__%d = " (tvindex + i))
        (fun ff -> Format.fprintf ff ";@,")
      e) args;
    Format.fprintf ff "%t(%s(%a))%t" rf1 (get_fun f)
     (Format.pp_print_list ~pp_sep:pp_sep_comma (fun ff x -> Format.fprintf ff "tmp__%d" x))
     (range tvindex (tvindex + (List.length args))) rf2
  | EIf (e1, e2, e3) ->
    print_expr ff env tvindex (fun ff -> Format.fprintf ff "tmp = ") (fun ff -> Format.fprintf ff ";@,") e1;
    Format.fprintf ff "@[<v 2>if (tmp != Val_false) {@,";
    print_expr ff env tvindex rf1 rf2 e2;
    Format.fprintf ff "@]@,@[<v 2>} else {@,";
    print_expr ff env tvindex rf1 rf2 e3;
    Format.fprintf ff "@]@,}@,"
  | EChain (e1, e2) ->
    print_expr ff env tvindex (fun ff -> Format.fprintf ff "tmp = ") (fun ff -> Format.fprintf ff ";@,") e1;
    print_expr ff env tvindex rf1 rf2 e2;
  | EMatch (e, l) ->
    print_expr ff env tvindex (fun ff -> Format.fprintf ff "tmp = ") (fun ff -> Format.fprintf ff ";@,") e;
    let no_arg, with_arg, default = split_pattern_matching l in
    assert (l <> []);
    let print_default () =
      let (vn, e) = match default with Some vn -> vn | None -> assert false in
      if vn = "_" then begin
        print_expr ff env tvindex rf1 rf2 e
      end else begin
        Format.fprintf ff "tmp__%d = tmp;@," tvindex;
        print_expr ff (SMap.add vn tvindex env) (tvindex + 1) rf1 rf2 e
      end
    in
    if (no_arg = [] && with_arg = []) then begin
      print_default ()
    end else begin
      let has_def = default <> None in
      let lab_def = if has_def then gen_label () else "BUG" in
      let do_def ff = if has_def then Format.fprintf ff "goto %s;" lab_def else Format.fprintf ff "assert(0);" in
      Format.fprintf ff "@[<v 2>if (Is_long(tmp)) { switch (Int_val(tmp)) {@,";
      List.iter (fun (c, e) ->
        Format.fprintf ff "@[<v 2>case tag__%s:@," c;
        print_expr ff env tvindex rf1 rf2 e;
        Format.fprintf ff "@]@,"
      ) no_arg;
      Format.fprintf ff "default: %t@]@,@[<v 2>}} else { switch (Tag_val(tmp)) {@," do_def;
      List.iter (fun (c, l, e) ->
        Format.fprintf ff "@[<v 2>case tag__%s:@," c;
        let rec set_variables tvindex env i = function
          | [] -> tvindex, env
          | "_" :: l -> set_variables tvindex env (i + 1) l
          | x :: l ->
            Format.fprintf ff "tmp__%d = Field(tmp, %d);@," tvindex i;
            set_variables (tvindex + 1) (SMap.add x tvindex env) (i + 1) l
        in
        let ntv, nenv = set_variables tvindex env 0 l in
        print_expr ff nenv ntv rf1 rf2 e;
        Format.fprintf ff "@]@,"
      ) with_arg;
      Format.fprintf ff "default: %t@]@,}}@," do_def;
      if has_def then begin
        Format.fprintf ff "@[<v 2>if (0) { %s:@," lab_def;
        print_default ();
        Format.fprintf ff "@]@,}@,"
      end
    end
  | ELet (p, e1, e2) -> print_expr ff env tvindex rf1 rf2 (EMatch (e1, [(p, e2)]))

let print_def ff = function
  | MLet (name, args, body) when args <> [] ->
    Format.fprintf ff "@[<v 2>value %s(%a) {@," name
        (Format.pp_print_list
           ~pp_sep:pp_sep_comma
           (fun ff x -> Format.fprintf ff "value %s" x)) args;
    Format.fprintf ff "value tmp;@,";
    Format.fprintf ff "CAMLparam%d(%a);@," (List.length args) (Format.pp_print_list ~pp_sep:pp_sep_comma Format.pp_print_string) args;
    let tv = expr_tempvars body in
    Format.fprintf ff "%a@," print_tempvars tv;
    print_expr ff SMap.empty 0
      (fun ff -> Format.fprintf ff "CAMLdrop; return ")
      (fun ff -> Format.fprintf ff ";@,")
      body;
    Format.fprintf ff "@]@,}@.@."    
  | _ -> ()

let print_init ff = function
  | MLet (name, [], body) ->
    Format.fprintf ff "%s =@," name;
    assert false
  | _ -> ()


let compile_and_print ff defs =
  List.iter (print_decl ff) defs;
  List.iter (print_def ff) defs;
  Format.fprintf ff "@[<v 2>void init() {@,";
  List.iter (print_init ff) defs;
  Format.fprintf ff "@]@,}@."