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

(* $Id: gMisc.ml 1527 2010-09-09 08:02:22Z garrigue $ *)

open Gaux
open Gobject
open Gtk
open GtkBase
open GtkMisc
open OgtkMiscProps
open GObj

let separator dir ?packing ?show () =
  let w = Separator.create dir [] in
  pack_return (new widget_full w) ~packing ~show

class statusbar_context obj ctx = object (self)
  val obj : statusbar obj = obj
  val context : Gtk.statusbar_context = ctx
  method context = context
  method push text = Statusbar.push obj context ~text
  method pop () = Statusbar.pop obj context
  method remove = Statusbar.remove obj context
  method flash ?(delay=1000) text =
    let msg = self#push text in
    Glib.Timeout.add delay (fun () -> self#remove msg; false);
    ()
end

class statusbar obj = object
  inherit GPack.box (obj : Gtk.statusbar obj)
  method has_resize_grip = Statusbar.get_has_resize_grip obj
  method set_has_resize_grip v = Statusbar.set_has_resize_grip obj v
  method new_context ~name =
    new statusbar_context obj (Statusbar.get_context_id obj name)
end

let statusbar =
  Statusbar.make_params [] ~cont:
    (GContainer.pack_container ~create:
       (fun p -> new statusbar (Statusbar.create p)))

class misc obj = object
  inherit ['a] widget_impl obj
  inherit misc_props
end

class image obj = object (self)
  inherit misc obj
  inherit image_props
  method clear () = Image.clear obj
end

type image_type =
  [ `EMPTY | `PIXMAP | `IMAGE | `PIXBUF | `STOCK | `ICON_SET | `ANIMATION ]

let image =
  Image.make_params [] ~cont:(
  Misc.all_params ~cont:(fun p ?packing ?show () ->
    pack_return (new image (Image.create p)) ~packing ~show))

let pixmap pm =
  let pl = [param Image.P.pixmap pm#pixmap; param Image.P.mask pm#mask] in
  Misc.all_params pl ~cont:(fun pl ?packing ?show () ->
    pack_return (new image (Image.create pl)) ~packing ~show)

class label_skel obj = object(self)
  inherit misc obj
  inherit label_props
  method text = GtkMiscProps.Label.get_text obj
  method set_text = GtkMiscProps.Label.set_text obj
  method selection_bounds = GtkMiscProps.Label.get_selection_bounds obj
  method select_region = GtkMiscProps.Label.select_region obj
end

class label obj = object
  inherit label_skel (obj : Gtk.label obj)
  method connect = new widget_signals_impl obj
end

let label ?text ?markup ?use_underline ?mnemonic_widget =
  let label, use_markup =
    if markup = None then text, None else markup, Some true in
  let mnemonic_widget = may_map (fun w -> w#as_widget) mnemonic_widget in
  Label.make_params [] ?label ?use_markup ?use_underline ?mnemonic_widget
    ~cont:(
  Misc.all_params ~cont:(fun p ?packing ?show () ->
    pack_return (new label (Label.create p)) ~packing ~show))

let label_cast w = new label (Label.cast w#as_widget)

class tips_query_signals obj = object
  inherit widget_signals_impl (obj : Gtk.tips_query obj)
  inherit tips_query_sigs
end

class tips_query obj = object
  inherit label_skel obj
  method start () = TipsQuery.start_query obj
  method stop () = TipsQuery.stop_query obj
  inherit tips_query_props
  method connect = new tips_query_signals obj
end

let tips_query ?caller =
  let caller = may_map (fun w -> w#as_widget) caller in
  TipsQuery.make_params [] ?caller ~cont:(
  Misc.all_params ~cont:(fun p ?packing ?show () ->
    pack_return (new tips_query (TipsQuery.create p)) ~packing ~show))
