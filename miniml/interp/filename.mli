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

(** Operations on file names. *)

val current_dir_name : string
(** The conventional name for the current directory (e.g. [.] in Unix). *)

val parent_dir_name : string
(** The conventional name for the parent of the current directory
   (e.g. [..] in Unix). *)

val dir_sep : string
(** The directory separator (e.g. [/] in Unix). @since 3.11.2 *)

val concat : string -> string -> string
(** [concat dir file] returns a file name that designates file
   [file] in directory [dir]. *)

val is_relative : string -> bool
(** Return [true] if the file name is relative to the current
   directory, [false] if it is absolute (i.e. in Unix, starts
   with [/]). *)

val is_implicit : string -> bool
(** Return [true] if the file name is relative and does not start
   with an explicit reference to the current directory ([./] or
   [../] in Unix), [false] if it starts with an explicit reference
   to the root directory or the current directory. *)

val check_suffix : string -> string -> bool
(** [check_suffix name suff] returns [true] if the filename [name]
   ends with the suffix [suff]. *)

val chop_suffix : string -> string -> string
(** [chop_suffix name suff] removes the suffix [suff] from
   the filename [name]. The behavior is undefined if [name] does not
   end with the suffix [suff]. *)

val extension : string -> string
(** [extension name] is the shortest suffix [ext] of [name0] where:

    - [name0] is the longest suffix of [name] that does not
      contain a directory separator;
    - [ext] starts with a period;
    - [ext] is preceded by at least one non-period character
      in [name0].

    If such a suffix does not exist, [extension name] is the empty
    string.

    @since 4.04
*)

val remove_extension : string -> string
(** Return the given file name without its extension, as defined
    in {!Filename.extension}. If the extension is empty, the function
    returns the given file name.

    The following invariant holds for any file name [s]:

    [remove_extension s ^ extension s = s]

    @since 4.04
*)

val chop_extension : string -> string
(** Same as {!Filename.remove_extension}, but raise [Invalid_argument]
    if the given name has an empty extension. *)


val basename : string -> string
(** Split a file name into directory name / base file name.
   If [name] is a valid file name, then [concat (dirname name) (basename name)]
   returns a file name which is equivalent to [name]. Moreover,
   after setting the current directory to [dirname name] (with {!Sys.chdir}),
   references to [basename name] (which is a relative file name)
   designate the same file as [name] before the call to {!Sys.chdir}.

   This function conforms to the specification of POSIX.1-2008 for the
   [basename] utility. *)

val dirname : string -> string
(** See {!Filename.basename}.
   This function conforms to the specification of POSIX.1-2008 for the
   [dirname] utility. *)

val temp_dir_name : string
  [@@ocaml.deprecated "Use Filename.get_temp_dir_name instead"]
(** The name of the initial temporary directory:
    Under Unix, the value of the [TMPDIR] environment variable, or "/tmp"
    if the variable is not set.
    Under Windows, the value of the [TEMP] environment variable, or "."
    if the variable is not set.
    @deprecated You should use {!Filename.get_temp_dir_name} instead.
    @since 3.09.1
*)

val quote : string -> string
(** Return a quoted version of a file name, suitable for use as
    one argument in a command line, escaping all meta-characters.
    Warning: under Windows, the output is only suitable for use
    with programs that follow the standard Windows quoting
    conventions.
 *)
