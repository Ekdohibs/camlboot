open Asttypes
open Conf
open Data

let empty_env = (SMap.empty, SMap.empty, SMap.empty)

let set_env env = function
  | Fun (_, _, _, _, ev) | Function (_, ev) -> ev := env
  | _ -> assert false

let rec env_get_module ((_, module_env, _) as env) { txt = lident; loc } =
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

let env_get_value ((value_env, _, _) as env) { txt = lident; loc } =
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

let env_get_constr ((_, _, constr_env) as env) { txt = lident; loc } =
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

let env_set_value key v (ve, me, ce) = (SMap.add key (true, v) ve, me, ce)
let env_set_module key m (ve, me, ce) = (ve, SMap.add key (true, m) me, ce)
let env_set_constr key c (ve, me, ce) = (ve, me, SMap.add key (true, c) ce)

let env_extend exported (ve, me, ce) (ve1, me1, ce1) =
  let nve = SMap.fold (fun key v ve -> SMap.add key (exported, v) ve) ve1 ve in
  let nme = SMap.fold (fun key m me -> SMap.add key (exported, m) me) me1 me in
  let nce = SMap.fold (fun key c ce -> SMap.add key (exported, c) ce) ce1 ce in
  (nve, nme, nce)

let make_module (ve, me, ce) =
  let ve = SMap.map snd (SMap.filter (fun _ (b, _) -> b) ve) in
  let me = SMap.map snd (SMap.filter (fun _ (b, _) -> b) me) in
  let ce = SMap.map snd (SMap.filter (fun _ (b, _) -> b) ce) in
  Module (ve, me, ce)

let prevent_export (ve, me, ce) =
  let ve = SMap.map (fun (_, x) -> (false, x)) ve in
  let me = SMap.map (fun (_, x) -> (false, x)) me in
  let ce = SMap.map (fun (_, x) -> (false, x)) ce in
  (ve, me, ce)
