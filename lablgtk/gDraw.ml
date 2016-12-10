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

(* $Id: gDraw.ml 1347 2007-06-20 07:40:34Z guesdon $ *)

open Gaux
open Gobject
open Gdk

type color = [
  | `COLOR of Gdk.color
  | `WHITE
  | `BLACK
  | `NAME of string
  | `RGB of int * int * int
]

let default_colormap = GtkBase.Widget.get_default_colormap

let color ?(colormap = default_colormap ()) (c : color) =
  match c with
  | `COLOR col -> col
  | #Gdk.Color.spec as def -> Color.alloc ~colormap def

let conv_color : color data_conv =
  { kind = `POINTER;
    proj = (function `POINTER (Some c) -> `COLOR (Obj.magic c)
           | _ -> failwith "GDraw.get_color");
    inj = (fun c -> `POINTER (Some (Obj.magic (color c : Gdk.color)))) }

type optcolor = [
  | `COLOR of Gdk.color
  | `WHITE
  | `BLACK
  | `NAME of string
  | `RGB of int * int * int
  | `DEFAULT
]

let optcolor ?colormap (c : optcolor) =
  match c with
  | `DEFAULT -> None
  | #color as c -> Some (color ?colormap c)

let conv_optcolor : optcolor data_conv =
  { kind = `POINTER;
    proj = (function `POINTER (Some c) -> `COLOR (Obj.magic c)
           | `POINTER None -> `DEFAULT
           | _ -> failwith "GDraw.get_color");
    inj = (fun c -> `POINTER (Obj.magic (optcolor c : Gdk.color option))) }

