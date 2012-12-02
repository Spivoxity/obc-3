(*
 * stackview.ml
 * 
 * This file is part of the Oxford Oberon-2 compiler
 * Copyright (c) 2008 J. M. Spivey
 * All rights reserved
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 * 1. Redistributions of source code must retain the above copyright notice,
 *    this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright notice,
 *    this list of conditions and the following disclaimer in the documentation
 *    and/or other materials provided with the distribution.
 * 3. The name of the author may not be used to endorse or promote products
 *    derived from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 * OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
 * IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS;
 * OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
 * WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
 * OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
 * ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *)

open Data
open Print
open Binary

class type stackview =
  object
    method backtrace : unit -> unit
    method clear : unit -> unit
    method as_widget: GObj.widget
  end

class stackview_impl
    (notebook : Sourcebook.sourcebook)  
    (vars : Varsview.varsview) 
    (peer : GTree.view) =
  let cols = new GTree.column_list in
  let c_regs = cols#add Gobject.Data.caml in
  let c_frame = cols#add Gobject.Data.string in
  let store = GTree.list_store cols in

  let vc_frame = GTree.view_column ~title:"Frame" 
    ~renderer:(GTree.cell_renderer_text [], ["text", c_frame]) () in

  let depth = ref 0 in

  object (self)
    method as_widget = (peer :> GObj.widget)

    method backtrace () =
      store#clear ();
      Control.backtrace (fun j rs ->
	  ignore (let iter = store#prepend () in
	    store#set ~row:iter ~column:c_regs rs;
	    store#set ~row:iter ~column:c_frame (sprintf "$" [fFrame rs])));
      depth := store#iter_n_children None;
      if !depth > 0 then begin
	let current = store#iter_children ~nth:(!depth-1) None in
	peer#selection#select_iter current;
	peer#scroll_to_cell ~align:(0.0, 0.2) (store#get_path current) vc_frame
      end

    method private select_frame path =
      try
        let iter = store#get_iter path in
	let f = store#get ~row:iter ~column:c_regs in
	let loc = find_loc f in
	notebook#goto_loc loc false;
	vars#show_frame f
      with Not_found -> ()

    method clear () = store#clear ()

    method private click () =
      let rows = peer#selection#get_selected_rows in
      if rows <> [] then
	self#select_frame (List.hd rows)

  initializer
    peer#misc#modify_font_by_name Debconf.sans_font;
    peer#set_model (Some store#coerce);
    peer#set_headers_visible false;
    peer#selection#set_mode `SINGLE;
    ignore (peer#selection#connect#changed self#click);

    ignore (peer#append_column vc_frame)
  end

let stackview notebook vars ?width ?height ?packing () =
  new stackview_impl notebook vars 
    (GTree.view ?width ?height ?packing ())

