(**************************************************************************)
(*                Lablgtk                                                 *)
(*                                                                        *)
(*    This program is free software; you can redistribute it              *)
(*    and/or modify it under the terms of the GNU Library General         *)
(*    Public License as published by the Free Software Foundation         *)
(*    version 2, with the exception described in file COPYING which       *)
(*    comes with the library.                                             *)
(*                                                                        *)
(*    This program is distributed in the hope that it will be useful,     *)
(*    but WITHOUT ANY WARRANTY; without even the implied warranty of      *)
(*    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the       *)
(*    GNU Library General Public License for more details.                *)
(*                                                                        *)
(*    You should have received a copy of the GNU Library General          *)
(*    Public License along with this program; if not, write to the        *)
(*    Free Software Foundation, Inc., 59 Temple Place, Suite 330,         *)
(*    Boston, MA 02111-1307  USA                                          *)
(*                                                                        *)
(*                                                                        *)
(**************************************************************************)

(* $Id: glib.mli 1408 2008-07-23 12:40:06Z ben_99_9 $ *)

(** Interface to Glib functions 
    @gtkdoc glib index *)

type unichar = int
type unistring = unichar array

exception GError of string

(** {3 Main Event Loop} *)

(** The Main Event Loop 
    @gtkdoc glib glib-The-Main-Event-Loop *)
module Main : sig
  type t
  val create : bool -> t
  val iteration : bool -> bool
  val pending : unit -> bool
  val is_running : t -> bool
  val quit : t -> unit
  type locale_category =
    [ `ALL | `COLLATE | `CTYPE | `MESSAGES | `MONETARY | `NUMERIC | `TIME ]
  val setlocale : locale_category -> string option -> string 
end

(** @gtkdoc glib glib-The-Main-Event-Loop *)
module Timeout : sig
  type id
  val add : ms:int -> callback:(unit -> bool) -> id
  val remove : id -> unit
end

(** @gtkdoc glib glib-The-Main-Event-Loop *)
module Idle : sig
  type id
  val add : ?prio:int -> (unit -> bool) -> id
  val remove : id -> unit
end

(** {3 Miscellaneous Utility Functions} *)

val get_prgname : unit -> string
val set_prgname : string -> unit
val get_application_name : unit -> string (** @since GTK 2.2 *)
val set_application_name : string -> unit (** @since GTK 2.2 *)

val get_user_name : unit -> string
val get_real_name : unit -> string

val get_home_dir : unit -> string option
val get_tmp_dir  : unit -> string

val find_program_in_path : string -> string
    (** @raise Not_found if the program is not found in the path
        or is not executable *)

val getenv : string -> string
    (** @raise Not_found if the environment variable is not found. *)
val setenv : string -> string -> bool -> unit
    (** @raise Failure if the environment variable couldn't be set.
        @since GTK 2.4 *)
val unsetenv : string -> unit
    (** @since GTK 2.4 *)

val get_user_cache_dir : unit -> string (** @since GTK 2.6 *)
val get_user_data_dir : unit -> string (** @since GTK 2.6 *)
val get_user_config_dir : unit -> string (** @since GTK 2.6 *)
val get_system_data_dirs : unit -> string list (** @since GTK 2.6 *)
val get_system_config_dirs : unit -> string list (** @since GTK 2.6 *)

val usleep : int -> unit
