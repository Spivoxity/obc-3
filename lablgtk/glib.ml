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

(* $Id: glib.ml 1503 2010-05-18 07:10:39Z garrigue $ *)

type unichar = int
type unistring = unichar array

exception GError of string
external _init : unit -> unit = "ml_glib_init"
let () =  _init () ; Callback.register_exception "gerror" (GError "")

module Main = struct
  type t
  external create : bool -> t = "ml_g_main_new"
  external iteration : bool -> bool = "ml_g_main_iteration"
  external pending : unit -> bool = "ml_g_main_pending"
  external is_running : t -> bool = "ml_g_main_is_running"
  external quit : t -> unit = "ml_g_main_quit"
  type locale_category =
    [ `ALL | `COLLATE | `CTYPE | `MESSAGES | `MONETARY | `NUMERIC | `TIME ]
  external setlocale : locale_category -> string option -> string 
    = "ml_setlocale"
end

module Timeout = struct
  type id
  external add : ?prio:int -> ms:int -> callback:(unit -> bool) -> id
    = "ml_g_timeout_add"
  let add = add ?prio:None
  external remove : id -> unit = "ml_g_source_remove"
end

module Idle = struct
  type id
  external add : ?prio:int -> (unit -> bool) -> id
    = "ml_g_idle_add"
  external remove : id -> unit = "ml_g_source_remove"
end

module Io = struct
  type channel
  type condition = [ `IN | `OUT | `PRI | `ERR | `HUP | `NVAL ]
  type id
  external channel_of_descr : Unix.file_descr -> channel
    = "ml_g_io_channel_unix_new"
  external remove : id -> unit = "ml_g_source_remove"
  external add_watch :
    cond:condition list -> callback:(condition list -> bool) -> ?prio:int -> channel -> id
    = "ml_g_io_add_watch"
  external read : channel -> buf:string -> pos:int -> len:int -> int
    = "ml_g_io_channel_read"
  external read_chars : channel -> buf:string -> pos:int -> len:int -> int
    = "ml_g_io_channel_read_chars"
end

external get_prgname : unit -> string = "ml_g_get_prgname"
external set_prgname : string -> unit = "ml_g_set_prgname"
external get_application_name : unit -> string = "ml_g_get_application_name"
external set_application_name : string -> unit = "ml_g_set_application_name"

external get_user_name : unit -> string = "ml_g_get_user_name"
external get_real_name : unit -> string = "ml_g_get_real_name"

external get_home_dir : unit -> string option = "ml_g_get_home_dir"
external get_tmp_dir  : unit -> string = "ml_g_get_tmp_dir"
external find_program_in_path : string -> string = "ml_g_find_program_in_path"

external getenv : string -> string = "ml_g_getenv"
external setenv : string -> string -> bool -> unit = "ml_g_setenv"
external unsetenv : string -> unit = "ml_g_unsetenv"

external get_user_cache_dir : unit -> string = "ml_g_get_user_cache_dir"
external get_user_data_dir : unit -> string = "ml_g_get_user_data_dir"
external get_user_config_dir : unit -> string = "ml_g_get_user_config_dir"
external get_system_data_dirs : unit -> string list = "ml_g_get_system_data_dirs"
external get_system_config_dirs : unit -> string list = "ml_g_get_system_config_dirs"

external usleep : int -> unit = "ml_g_usleep"
