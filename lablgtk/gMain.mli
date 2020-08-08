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

(* $Id: gMain.mli 1408 2008-07-23 12:40:06Z ben_99_9 $ *)

(** Library initialization, main event loop, and events *)

open Gtk

(** @gtkdoc gtk gtk-General *)

(** [init] also sets the locale and returns its name.
       Either set [~setlocale] to [false] or GTK_SETLOCALE to "0"
       if you don't want to the locale to be set *)
val init : ?setlocale:bool -> unit -> string

(** [main] runs the main loop, until [quit] is called.
       {e Do not use in multi-threaded programs.} *)
val main : unit -> unit

(** quit the main loop *)
val quit : unit -> unit

(** [major, minor, micro] *)
val version : int * int * int

(** Specify a platform-dependent menubar *)
val set_platform_menubar : GMenu.menu_shell -> GMenu.menu_item -> unit


(** Global structures *)

val selection : GData.clipboard
val clipboard : GData.clipboard

module Grab : sig
  val add : #GObj.widget -> unit
  val remove : #GObj.widget -> unit
  val get_current : unit -> GObj.widget
end

module Event : sig
  val get_current_time : unit -> int32     (** May return GDK_CURRENT_TIME *)
  val get_current : unit -> GdkEvent.any       (** May raise Gpointer.Null *)
  val get_widget : 'a Gdk.event -> widget obj  (** May raise Gpointer.Null *)
  val propagate : [> `widget] obj -> 'a Gdk.event -> unit
end

module Rc : sig
  val add_default_file : string -> unit
end

module Timeout : sig
  type id = Glib.Timeout.id
  val add : ms:int -> callback:(unit -> bool) -> id
  val remove : id -> unit
end

module Idle : sig
  type id = Glib.Idle.id
  val add : ?prio:int -> (unit -> bool) -> id
  val remove : id -> unit
end
