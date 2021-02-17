(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Lexing

let absname = ref false
    (* This reference should be in Clflags, but it would create an additional
       dependency and make bootstrapping Camlp4 more difficult. *)

type t = { loc_start: position; loc_end: position; loc_ghost: bool };;

let in_file name =
  let loc = {
    pos_fname = name;
    pos_lnum = 1;
    pos_bol = 0;
    pos_cnum = -1;
  } in
  { loc_start = loc; loc_end = loc; loc_ghost = true }
;;

let none = in_file "_none_";;

let curr lexbuf = {
  loc_start = lexbuf.lex_start_p;
  loc_end = lexbuf.lex_curr_p;
  loc_ghost = false
};;

let init lexbuf fname =
  lexbuf.lex_curr_p <- {
    pos_fname = fname;
    pos_lnum = 1;
    pos_bol = 0;
    pos_cnum = 0;
  }
;;

let symbol_rloc () = {
  loc_start = Parsing.symbol_start_pos ();
  loc_end = Parsing.symbol_end_pos ();
  loc_ghost = false;
};;

let symbol_gloc () = {
  loc_start = Parsing.symbol_start_pos ();
  loc_end = Parsing.symbol_end_pos ();
  loc_ghost = true;
};;

let rhs_loc n = {
  loc_start = Parsing.rhs_start_pos n;
  loc_end = Parsing.rhs_end_pos n;
  loc_ghost = false;
};;

let input_name = ref "_none_"
let input_lexbuf = ref (None : lexbuf option)


let print_filename ppf file =
  Format.fprintf ppf "%s" file

let msg_file = "File \""
let msg_line = "\", line "
let msg_chars = ", characters "
let msg_to = "-"
let msg_colon = ":"

(* return file, line, char from the given position *)
let get_pos_info pos =
  (pos.pos_fname, pos.pos_lnum, pos.pos_cnum - pos.pos_bol)
;;

let setup_colors () = ()

let print_loc ppf loc =
  let (file, line, startchar) = get_pos_info loc.loc_start in
  let endchar = loc.loc_end.pos_cnum - loc.loc_start.pos_cnum + startchar in
  Format.fprintf ppf "%s%a%s%d" msg_file print_filename file msg_line line;
  if startchar >= 0 then
    Format.fprintf ppf "%s%d%s%d" msg_chars startchar msg_to endchar

let error_prefix = "Error"
let warning_prefix = "Warning"

let print_error ppf loc =
  Format.fprintf ppf "%a%s:" print_loc loc error_prefix

let print_error_cur_file ppf () = print_error ppf (in_file !input_name);;

let print_warning loc ppf w = ()
let prerr_warning loc w = ()

type 'a loc = {
  txt : 'a;
  loc : t;
}

let mkloc txt loc = { txt=txt ; loc=loc }
let mknoloc txt = mkloc txt none

let register_error_of_exn f = ()
let deprecated ?(def = none) ?(use = none) loc msg = ()
let error_of_printer loc print x = assert false
