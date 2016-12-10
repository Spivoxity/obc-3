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

(* $Id: gToolbox.ml 1347 2007-06-20 07:40:34Z guesdon $ *)

open StdLabels

(** Menus *)

type menu_entry =
  [ `I of string * (unit -> unit)
  | `C of string * bool * (bool -> unit)
  | `R of (string * bool * (bool -> unit)) list
  | `M of string * menu_entry list
  | `S ]

let rec build_menu menu ~(entries : menu_entry list) =
  let f = new GMenu.factory menu in
  List.iter entries ~f:
    begin function
    | `I (label, callback) ->
        ignore (f#add_item label ~callback)
    | `C (label, active, callback) ->
        ignore (f#add_check_item label ~callback ~active)
    | `R ((label, active, callback) :: l) ->
        let r = f#add_radio_item label ~active ~callback in
        let group = r#group in
        List.iter l ~f:
          (fun (label, active, callback) ->
            ignore (f#add_radio_item label ~active ~callback ~group))
    | `R [] ->
        ()
    | `M (label, entries) ->
        let m = f#add_submenu label in
        build_menu m ~entries
    | `S ->
        ignore (f#add_separator ())
    end

let popup_menu ~entries =
  let menu = GMenu.menu () in
  build_menu menu ~entries;
  fun ~button ~time -> 
    if entries = [] then 
      () 
    else 
      menu#popup ~button ~time

(** Dialogs *)

let mOk = "Ok"
let mCancel = "Cancel"

let question_box ~title  ~buttons ?(default=1) ?icon message =
  let button_nb = ref 0 in
  let window = GWindow.dialog ~modal:true ~title () in
  let hbox = GPack.hbox ~border_width:10 ~packing:window#vbox#add () in
  let bbox = window#action_area in
  begin match icon with
    None -> ()
  | Some i -> hbox#pack i#coerce ~padding:4
  end;
  ignore (GMisc.label ~text: message ~packing: hbox#add ());
  (* the function called to create each button by iterating *)
  let rec iter_buttons n = function
      [] ->
        ()
    | button_label :: q ->    
        let b = GButton.button ~label: button_label 
            ~packing:(bbox#pack ~expand:true ~padding:4) ()
        in
        b#connect#clicked ~callback:
          (fun () -> button_nb := n; window#destroy ());
        (* If it's the first button then give it the focus *)
        if n = default then b#grab_default () else ();

        iter_buttons (n+1) q
  in
  iter_buttons 1 buttons;
  window#connect#destroy ~callback: GMain.quit;
  window#set_position `CENTER;
  window#show ();
  GMain.main ();
  !button_nb


let message_box ~title ?icon ?(ok=mOk) message =
  ignore (question_box ?icon ~title message ~buttons:[ ok ])
