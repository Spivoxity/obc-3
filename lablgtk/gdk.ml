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

(* $Id: gdk.ml 1452 2009-05-08 10:15:38Z garrigue $ *)

open StdLabels
open Gaux
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
let _ = Callback.register_exception "gdkerror" (Error"")

external _gdk_init : unit -> unit = "ml_gdk_init"
let () = _gdk_init ()

module Tags = struct
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

  type extension_mode =
    [ `NONE | `ALL | `CURSOR ]

  type visibility_state =
    [ `UNOBSCURED | `PARTIAL | `FULLY_OBSCURED ]

  type input_source =
    [ `MOUSE | `PEN | `ERASER | `CURSOR ]

  type scroll_direction =
    [ `UP | `DOWN | `LEFT | `RIGHT ]

  type notify_type =
    [ `ANCESTOR | `VIRTUAL | `INFERIOR | `NONLINEAR
    | `NONLINEAR_VIRTUAL | `UNKNOWN ] 

  type crossing_mode = [ `NORMAL | `GRAB | `UNGRAB ]

  type setting_action = [ `NEW | `CHANGED | `DELETED ]

  type window_state =
    [ `WITHDRAWN | `ICONIFIED | `MAXIMIZED | `STICKY ]

  type modifier =
    [ `SHIFT | `LOCK | `CONTROL | `MOD1 | `MOD2 | `MOD3 | `MOD4 | `MOD5
    | `BUTTON1 | `BUTTON2 | `BUTTON3 | `BUTTON4 | `BUTTON5 ]

  type drag_action =
    [ `DEFAULT | `COPY | `MOVE | `LINK | `PRIVATE | `ASK ]

  type rgb_dither = 
    [ `NONE | `NORMAL | `MAX]

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
open Tags

module Convert = struct
  external test_modifier : modifier -> int -> bool
      = "ml_test_GdkModifier_val"
  let modifier i =
    List.filter [`SHIFT;`LOCK;`CONTROL;`MOD1;`MOD2;`MOD3;`MOD4;`MOD5;
		 `BUTTON1;`BUTTON2;`BUTTON3;`BUTTON4;`BUTTON5]
      ~f:(fun m -> test_modifier m i)
  external test_window_state : window_state -> int -> bool
      = "ml_test_GdkWindowState_val"
  let window_state i =
    List.filter [ `WITHDRAWN; `ICONIFIED; `MAXIMIZED; `STICKY ]
      ~f:(fun m -> test_window_state m i)
end

module Atom = struct
  external intern : string -> bool -> atom = "ml_gdk_atom_intern"
  let intern ?(dont_create=false) name = intern name dont_create
  external name : atom -> string = "ml_gdk_atom_name"
  let none = intern "NONE"
  let primary = intern "PRIMARY"
  let secondary = intern "SECONDARY"
  let clipboard = intern "CLIPBOARD"
  let string = intern "STRING"
end

module Property = struct
  external change :
      window ->
      property:atom -> typ:atom -> mode:property_mode -> xdata -> unit
      = "ml_gdk_property_change"
  let change ~window ~typ ?(mode=`REPLACE) property data =
    change window ~property ~typ ~mode data
  external get :
      window -> property:atom ->
      max_length:int -> delete:bool -> (atom * xdata) option
      = "ml_gdk_property_get"
  let get ~window ?(max_length=65000) ?(delete=false) property =
    get window ~property ~max_length ~delete
  external delete : window:window -> atom -> unit
      = "ml_gdk_property_delete"
end

module Color = struct
  external color_white : colormap -> color = "ml_gdk_color_white"
  external color_black : colormap -> color = "ml_gdk_color_black"
  external color_parse : string -> color = "ml_gdk_color_parse"
  external color_alloc : colormap -> color -> bool = "ml_gdk_color_alloc"
  external color_create : red:int -> green:int -> blue:int -> color
      = "ml_GdkColor"

  external get_system_colormap : unit -> colormap
      = "ml_gdk_colormap_get_system"
  external colormap_new : visual -> privat:bool -> colormap
      = "ml_gdk_colormap_new"
  let get_colormap ?(privat=false) vis = colormap_new vis ~privat
  external get_visual : colormap -> visual
      = "ml_gdk_colormap_get_visual"

  type spec = [ `BLACK | `NAME of string | `RGB of int * int * int | `WHITE]
  let color_alloc ~colormap color =
    if not (color_alloc colormap color) then raise (Error"Color.alloc");
    color
  let alloc ~colormap color =
    match color with
      `WHITE -> color_white colormap
    | `BLACK -> color_black colormap
    | `NAME s -> color_alloc ~colormap (color_parse s)
    | `RGB (red,green,blue) ->
	color_alloc ~colormap (color_create ~red ~green ~blue)

  external red : color -> int = "ml_GdkColor_red"
  external blue : color -> int = "ml_GdkColor_blue"
  external green : color -> int = "ml_GdkColor_green"
  external pixel : color -> int = "ml_GdkColor_pixel"
end

module Rectangle = struct
  type t
  external create : x:int -> y:int -> width:int -> height:int -> t
      = "ml_GdkRectangle"
  external x : t -> int = "ml_GdkRectangle_x"
  external y : t -> int = "ml_GdkRectangle_y"
  external width : t -> int = "ml_GdkRectangle_width"
  external height : t -> int = "ml_GdkRectangle_height"
end
