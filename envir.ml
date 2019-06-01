open Asttypes
open Conf
open Data

let empty_env = {
  values = SMap.empty;
  modules = SMap.empty;
  constructors = SMap.empty;
}

let set_env env = function
  | Fun (_, _, _, _, ev) | Function (_, ev) -> ev := env
  | _ -> assert false

let rec env_get_module env { txt = lident; loc } =
  let module_env = env.modules in
  match lident with
  | Longident.Lident str ->
    (try snd (SMap.find str module_env)
     with Not_found ->
       if debug
       then
         Format.eprintf
           "%a@.Module not found in env: %s@."
           Location.print_loc
           loc
           str;
       raise Not_found)
  | Longident.Ldot (ld, str) ->
    let md = env_get_module env { txt = ld; loc } in
    (match md with
    | Functor _ -> failwith "Ldot tried to access functor"
    | Module (_, md, _) ->
      (try SMap.find str md
       with Not_found ->
         if debug
         then
           Format.eprintf
             "%a@.Module not found in submodule: %s@."
             Location.print_loc
             loc
             (String.concat "." (Longident.flatten lident));
         raise Not_found))
  | Longident.Lapply _ -> failwith "Lapply lookups not supported"

let env_get_value env { txt = lident; loc } =
  let value_env = env.values in
  match lident with
  | Longident.Lident str ->
    (try snd (SMap.find str value_env)
     with Not_found ->
       if debug
       then
         Format.eprintf
           "%a@.Variable not found in env: %s@."
           Location.print_loc
           loc
           str;
       raise Not_found)
  | Longident.Ldot (ld, str) ->
    let md = env_get_module env { txt = ld; loc } in
    (match md with
    | Functor _ -> failwith "Ldot tried to access functor"
    | Module (md, _, _) ->
      (try SMap.find str md
       with Not_found ->
         if debug
         then
           Format.eprintf
             "%a@.Value not found in submodule: %s@."
             Location.print_loc
             loc
             (String.concat "." (Longident.flatten lident));
         raise Not_found))
  | Longident.Lapply _ -> failwith "Lapply lookups not supported"

let env_get_constr env { txt = lident; loc } =
  let constr_env = env.constructors in
  match lident with
  | Longident.Lident str ->
    (try snd (SMap.find str constr_env)
     with Not_found ->
       if debug
       then
         Format.eprintf
           "%a@.Constructor not found in env: %s@."
           Location.print_loc
           loc
           str;
       raise Not_found)
  | Longident.Ldot (ld, str) ->
    let md = env_get_module env { txt = ld; loc } in
    (match md with
    | Functor _ -> failwith "Ldot tried to access functor"
    | Module (_, _, md) ->
      (try SMap.find str md
       with Not_found ->
         if debug
         then
           Format.eprintf
             "%a@.Constructor not found in submodule: %s@."
             Location.print_loc
             loc
             (String.concat "." (Longident.flatten lident));
         raise Not_found))
  | Longident.Lapply _ -> failwith "Lapply lookups not supported"

let env_set_value key v env =
  { env with values = SMap.add key (true, v) env.values }
let env_set_module key m env =
  { env with modules = SMap.add key (true, m) env.modules }
let env_set_constr key c env =
  { env with constructors = SMap.add key (true, c) env.constructors }

let env_extend exported env1 (ve, me, ce) =
  let merge s1 s2 = SMap.fold (fun k v env -> SMap.add k (exported, v) env) s2 s1 in
  { values = merge env1.values ve;
    modules = merge env1.modules me;
    constructors = merge env1.constructors ce; }

let make_module env =
  let exported env_map =
    env_map |> SMap.filter (fun _ (b, _) -> b) |> SMap.map snd in
  Module (exported env.values, exported env.modules, exported env.constructors)

let prevent_export env =
  let prevent env_map = SMap.map (fun (_, x) -> (false, x)) env_map in
  { values = prevent env.values;
    modules = prevent env.modules;
    constructors = prevent env.constructors; }
