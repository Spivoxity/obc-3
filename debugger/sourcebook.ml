(*
 * sourcebook.ml
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

open GSourceView2
open Binary
open Print
open Symtab
open Dict

class type sourcebook =
  object
    method all_breakpoints : unit -> (Symtab.ident * int) list
    method goto_loc : Binary.source_loc -> bool -> unit
    method load_file : Symtab.ident -> string -> unit
    method set_click_enabled : bool -> unit
    method set_uglify : bool -> unit
    method show_module : Symtab.ident -> unit
    method uglify : bool
    method unmark : bool -> unit
    method as_widget : GObj.widget
  end

let find_break m n =
  let rec loop k =
    if k > n+3 then raise Not_found
    else if Util.can (Binary.line_lookup m) k then k
    else loop (k+1) in
  loop n

let file_contents fname =
  let chan = open_in_bin fname in
  let n = in_channel_length chan in
  let buf = Bytes.create n in
  really_input chan buf 0 n;
  close_in chan;
  Bytes.to_string buf

class debug_view m enabled 
    (buf : GSourceView2.source_buffer) 
    (peer : GSourceView2.source_view) =
  object (self)
    method source_buffer = buf

    method has_breakmark iter =
      buf#source_marks_at_iter ~category:"breakpoint" iter <> []

    method add_breakmark iter =
      ignore (buf#create_source_mark ~category:"breakpoint" iter)

    method remove_breakmark iter =
      buf#remove_source_marks ~category:"breakpoint" ~start:iter ~stop:iter ()

    method goto_line n hereflag =
      let iter = buf#get_iter_at_char ~line:(n-1) 0 in
      ignore (buf#place_cursor iter);
      ignore (GtkThread.async (fun () ->
	peer#scroll_to_mark ~within_margin:0.1 `INSERT;
	ignore (buf#create_source_mark ~category:"frame" iter)) ());
      if hereflag then
	ignore (buf#create_source_mark ~category:"here" iter)

    method unmark hereflag =
      let start = buf#start_iter and stop = buf#end_iter in
      buf#remove_source_marks ~category:"frame" ~start ~stop ();
      if hereflag then
        buf#remove_source_marks ~category:"here" ~start ~stop ()

    method breakpoints =
      let it = buf#start_iter in
      let rec loop acc =
	if buf#forward_iter_to_source_mark ~category:"breakpoint" it then
	  loop ((m, it#line+1) :: acc)
	else
	  acc in
      List.rev (loop [])

    method set_uglify flag = buf#set_highlight_syntax flag

    method source_click ev = 
      if peer#get_window_type (GdkEvent.get_window ev) <> `LEFT then 
	false
      else if not !enabled || GdkEvent.get_type ev <> `BUTTON_PRESS then 
	true
      else begin
	let (x, y) = 
	  peer#window_to_buffer_coords `LEFT
	    (truncate (GdkEvent.Button.x ev)) 
	    (truncate (GdkEvent.Button.y ev)) in
	let (iter, _) = peer#get_line_at_y y in
	if self#has_breakmark iter then begin
	  (* Clicked on a breakpoint: unset it *)
	  Control.set_break m (iter#line+1) false;
	  self#remove_breakmark iter
	end
	else begin
	  try
	    (* Find a line that will take a breakpoint *)
	    let n = find_break m (iter#line+1) in
	    let break = buf#get_iter_at_char ~line:(n-1) 0 in
	    if not (self#has_breakmark break) then begin
	      (* No break there yet: make one *)
	      Control.set_break m n true;
	      self#add_breakmark break;
	      Debconf.flash_message "Breakpoint at line $" [fNum n]
	    end
	  with Not_found -> ()
	end;
	true
      end

  initializer
    peer#misc#modify_font_by_name Debconf.mono_font;
    peer#set_mark_category_pixbuf "breakpoint" 
      (Some (Debconf.pixbuf_resource "breakpoint.png"));
    peer#set_mark_category_priority "breakpoint" 1;
    peer#set_mark_category_pixbuf "here" 
      (Some (Debconf.pixbuf_resource "here.png"));
    peer#set_mark_category_priority "here" 2;
    peer#set_mark_category_background "frame" 
      (Some (GDraw.color (`NAME "#FFFF88")));

    (* GtkSourceView 2.4.1 doesn't like mark categories that have no
       associated image (see Bugzilla #564714).  Here's a workaround: *)
    peer#set_mark_category_pixbuf "frame"
      (Some (Debconf.pixbuf_resource "blank.png"));

    ignore (peer#event#connect#button_press self#source_click);
  end

let debug_view m enabled buf ?width ?height ?packing () =
  new debug_view m enabled buf
    (GSourceView2.source_view ?width ?height ?packing
       ~source_buffer:buf ~show_line_marks:true ~show_line_numbers:true 
       ~editable:false ~cursor_visible:false ())

class sourcebook_impl (peer : GPack.notebook) =
  let pagetbl = Hashtbl.create 10 in

  let language =
    let mgr = GSourceView2.source_language_manager ~default:true in
    mgr#language "oberon" in

  let style_scheme =
    let mgr = GSourceView2.source_style_scheme_manager ~default:true in
    mgr#style_scheme "sober" in

  let uglify = ref true in
  let click_enabled = ref true in

  object (self)
    method as_widget = (peer :> GObj.widget)

    method load_file m fname =
      ignore (self#create_page m fname)

    method private create_page m fname =
      let scroller = GBin.scrolled_window 
	  ~hpolicy:`AUTOMATIC ~vpolicy:`AUTOMATIC () in
      let text = file_contents fname in
      let buf = 
	GSourceView2.source_buffer ~text ?language ?style_scheme 
	  ~highlight_syntax:!uglify ~highlight_matching_brackets:false () in
      buf#place_cursor buf#start_iter;
      let view = debug_view m click_enabled buf ~packing:scroller#add () in
      let tabbox = GPack.hbox () in
      ignore (GMisc.label ~text:(extern m) ~packing:tabbox#add ());
      let tabbtn = GButton.button ~relief:`NONE ~packing:tabbox#add () in
      tabbtn#misc#set_name "tab-close-button";
      ignore (tabbtn#connect#clicked 
	  ~callback:(fun ev -> scroller#misc#hide ()));
      let img = GMisc.image ~stock:`CLOSE ~icon_size:`MENU () in
      ignore (tabbtn#set_image img#coerce);
      let tab = peer#append_page ~tab_label:tabbox#coerce scroller#coerce in
      Hashtbl.add pagetbl m (tab, view);
      (tab, view)

    method private find_module m =
      try
	let (tab, view) = Hashtbl.find pagetbl m in
	let scroller = peer#get_nth_page tab in
	scroller#misc#show ();
	(tab, view)
      with Not_found ->
	(* Page not loaded yet *)
	try 
	  let fname = Info.module_source m in self#create_page m fname
	with Not_found ->
	  (* Can't find the file *)
	  Debconf.flash_message "Can't find source for module $" [fId m];
	  raise Not_found

    method show_module m =
      try 
	let (tab, view) = self#find_module m in
	peer#goto_page tab
      with Not_found -> ()

    method goto_loc loc hereflag =
      self#unmark hereflag;
      match loc with
	  Line (m, p, n) ->
	    begin try
	      let (tab, view) = self#find_module m in
	      peer#goto_page tab;
	      view#goto_line n hereflag;
	    with
	      Not_found -> ()
	    end
	| Proc p ->
	    begin try
	      let d = Info.get_debug p in
	      if d.d_line <> 0 then begin
		let (tab, view) = self#find_module d.d_module in
		peer#goto_page tab;
		view#goto_line d.d_line hereflag
	      end
	    with 
	      Not_found -> ()
	    end
	| _ -> ()

    method private iter f = 
      Hashtbl.iter (fun m (_, view) -> f m view) pagetbl

    (* Delete "frame" and (optionally) "here" marks from all buffers: 
       there's only one of each, but why bother to keep track of it? *)
    method unmark hereflag =
      self#iter (fun m view -> view#unmark hereflag)

    method all_breakpoints () =
      let bs = ref [] in
      self#iter (fun m view -> bs := view#breakpoints @ !bs);
      !bs

    method uglify = !uglify

    method set_uglify flag =
      uglify := flag;
      self#iter (fun _ view -> view#set_uglify flag)

    method set_click_enabled flag = 
      click_enabled := flag
  end

let sourcebook ?width ?height ?packing () =
  new sourcebook_impl (GPack.notebook ?width ?height ?packing ())
