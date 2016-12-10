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

(* $Id: gdk.mli 1452 2009-05-08 10:15:38Z garrigue $ *)

open Gobject

type color
type colormap
type visual
type screen = [`gdkscreen] obj
type region
type gc
type window = [`drawable|`gdkwindow] obj
type pixmap = [`drawable|`gdkpixmap] obj
type bitmap = [`drawable|`gdkpixmap|`gdkbitmap] obj
type image = [`gdkimage] obj
type atom
type keysym = int
type +'a event
type cursor
type device
type display

exception Error of string

module Tags : sig
  type event_type =
    [ `NOTHING | `DELETE | `DESTROY | `EXPOSE | `MOTION_NOTIFY
    | `BUTTON_PRESS | `TWO_BUTTON_PRESS | `THREE_BUTTON_PRESS | `BUTTON_RELEASE
    | `KEY_PRESS | `KEY_RELEASE
    | `ENTER_NOTIFY | `LEAVE_NOTIFY | `FOCUS_CHANGE
    | `CONFIGURE | `MAP | `UNMAP | `PROPERTY_NOTIFY
    | `SELECTION_CLEAR | `SELECTION_REQUEST | `SELECTION_NOTIFY
    | `PROXIMITY_IN | `PROXIMITY_OUT
    | `DRAG_ENTER | `DRAG_LEAVE | `DRAG_MOTION | `DRAG_STATUS
    | `DROP_START | `DROP_FINISHED | `CLIENT_EVENT | `VISIBILITY_NOTIFY
    | `NO_EXPOSE | `SCROLL | `WINDOW_STATE | `SETTING ]
  type event_mask =
    [ `EXPOSURE
    | `POINTER_MOTION | `POINTER_MOTION_HINT
    | `BUTTON_MOTION | `BUTTON1_MOTION | `BUTTON2_MOTION | `BUTTON3_MOTION
    | `BUTTON_PRESS | `BUTTON_RELEASE
    | `KEY_PRESS | `KEY_RELEASE
    | `ENTER_NOTIFY | `LEAVE_NOTIFY | `FOCUS_CHANGE
    | `STRUCTURE | `PROPERTY_CHANGE | `VISIBILITY_NOTIFY
    | `PROXIMITY_IN | `PROXIMITY_OUT
    | `SUBSTRUCTURE | `SCROLL
    | `ALL_EVENTS ]
  type extension_mode = [ `NONE | `ALL | `CURSOR ]
  type visibility_state = [ `UNOBSCURED | `PARTIAL | `FULLY_OBSCURED ]
  type input_source = [ `MOUSE | `PEN | `ERASER | `CURSOR ]
  type scroll_direction = [ `UP | `DOWN | `LEFT | `RIGHT ]
  type notify_type =
    [ `ANCESTOR | `VIRTUAL | `INFERIOR | `NONLINEAR
    | `NONLINEAR_VIRTUAL | `UNKNOWN ] 
  type crossing_mode = [ `NORMAL | `GRAB | `UNGRAB ]
  type setting_action = [ `NEW | `CHANGED | `DELETED ]
  type window_state = [ `WITHDRAWN | `ICONIFIED | `MAXIMIZED | `STICKY ]
  type modifier =
    [ `SHIFT | `LOCK | `CONTROL | `MOD1 | `MOD2 | `MOD3 | `MOD4 | `MOD5
    | `BUTTON1 | `BUTTON2 | `BUTTON3 | `BUTTON4 | `BUTTON5 ]
  type drag_action = [ `DEFAULT | `COPY | `MOVE | `LINK | `PRIVATE | `ASK ]
  type rgb_dither = [ `NONE | `NORMAL | `MAX]
  type property_state = [ `NEW_VALUE | `DELETE ]
  type property_mode = [ `REPLACE | `PREPEND | `APPEND ]
  type xdata =
    [ `BYTES of string
    | `SHORTS of int array
    | `INT32S of int32 array ]
  type xdata_ret = [ xdata | `NONE ]
  type gravity =
    [ `NORTH_WEST | `NORTH | `NORTH_EAST | `WEST | `CENTER | `EAST
    | `SOUTH_WEST | `SOUTH | `SOUTH_EAST | `STATIC ]
  type window_type_hint =
    [ `NORMAL | `DIALOG | `MENU | `TOOLBAR | `SPLASHSCREEN | `UTILITY
    | `DOCK | `DESKTOP ]
end

module Convert :
  sig
    val test_modifier : Tags.modifier -> int -> bool
    val modifier : int -> Tags.modifier list
    val window_state : int -> Tags.window_state list
  end

module Atom :
  sig
    (* Currently Gtk2 does not implement ?dont_create... *)
    val intern :  ?dont_create:bool -> string -> atom
    val name : atom -> string
    val none : atom
    val primary : atom
    val secondary : atom
    val clipboard : atom
    val string : atom
  end

module Property :
  sig
    val change :
      window:window -> typ:atom ->
      ?mode:Tags.property_mode -> atom -> Tags.xdata -> unit
    val get :
      window:window -> ?max_length:int ->
      ?delete:bool -> atom -> (atom * Tags.xdata) option
    val delete : window:window -> atom -> unit
  end

module Color :
  sig
    val get_system_colormap : unit -> colormap
    val get_colormap : ?privat:bool -> visual -> colormap
    val get_visual : colormap -> visual

    type spec = [
      | `BLACK
      | `NAME of string
      | `RGB of int * int * int
      | `WHITE
    ]
    val alloc : colormap:colormap -> spec -> color
    val red : color -> int
    val blue : color -> int
    val green : color -> int
    val pixel : color -> int
  end

module Rectangle :
  sig
    type t
    val create : x:int -> y:int -> width:int -> height:int -> t
    val x : t -> int
    val y : t -> int
    val width : t -> int
    val height : t -> int
  end
