open Asttypes
open Conf
open Data

let empty_env =
  { values = SMap.empty; modules = SMap.empty; constructors = SMap.empty }

let set_env env = function
  | Fun (_, _, _, _, ev) | Function (_, ev) -> ev := env
  | _ -> assert false

let env_set_value key v env =
  { env with values = SMap.add key (true, v) env.values }

let env_set_module key m env =
  { env with modules = SMap.add key (true, m) env.modules }

let env_set_constr key c env =
  { env with constructors = SMap.add key (true, c) env.constructors }

let env_extend exported env1 (ve, me, ce) =
  let merge s1 s2 =
    SMap.fold (fun k v env -> SMap.add k (exported, v) env) s2 s1
  in
  { values = merge env1.values ve;
    modules = merge env1.modules me;
    constructors = merge env1.constructors ce
  }

let env_of_module_data mod_data = env_extend true empty_env mod_data

let make_module_data env =
  let exported env_map =
    env_map |> SMap.filter (fun _ (b, _) -> b) |> SMap.map snd
  in
  (exported env.values, exported env.modules, exported env.constructors)

let make_module env = Module (make_module_data env)

let prevent_export env =
  let prevent env_map = SMap.map (fun (_, x) -> (false, x)) env_map in
  { values = prevent env.values;
    modules = prevent env.modules;
    constructors = prevent env.constructors
  }

let decompose env_get_module_data env { txt = lident; loc } =
  match lident with
  | Longident.Lapply _ -> failwith "Lapply lookups not supported"
  | Longident.Lident str -> ("env", env, str)
  | Longident.Ldot (ld, str) ->
    let md = env_get_module_data env { txt = ld; loc } in
    ("module", env_of_module_data md, str)

let lookup object_name ~env_name object_env { txt = str; loc } =
  try snd (SMap.find str object_env)
  with Not_found ->
    if debug
    then
      Format.eprintf
        "%a@.%s not found in %s: %s@."
        Location.print_loc
        loc
        (String.capitalize_ascii object_name)
        env_name
        str;
    raise Not_found

let rec env_get_module env ({ loc; _ } as lid) =
  let env_name, env, id = decompose env_get_module_data env lid in
  lookup "module" ~env_name env.modules { txt = id; loc }

and env_get_value env ({ loc; _ } as lid) =
  let env_name, env, id = decompose env_get_module_data env lid in
  lookup "value" ~env_name env.values { txt = id; loc }

and env_get_constr env ({ loc; _ } as lid) =
  let env_name, env, id = decompose env_get_module_data env lid in
  lookup "constructor" ~env_name env.constructors { txt = id; loc }

and env_get_module_data env ({ loc; _ } as id) =
  get_module_data loc (env_get_module env id)
