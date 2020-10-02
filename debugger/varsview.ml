(*
 * varsview.ml
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

open Symtab
open Binary
open Print
open Dict
open Data
open Util
open Mach
open Eval

type selector = 
    Name of ident 			(* Variable or field *)
  | Subs of int 			(* Array element *)
  | Module of ident 			(* Globals for module *)
  | DotDotDot				(* Elided array elements *)
  | Dummy				(* Dummy child for unexpanded node *)

let fSel = 
  function
      Name x -> fId x
    | Subs n -> fMeta "[$]" [fNum n]
    | Module m -> fMeta "Module $" [fId m]
    | DotDotDot -> fStr "..."
    | Dummy -> fStr "*dummy*"

let uparrow = "\xe2\x86\x91" (* in UTF-8 *)

(* 
For each module or procedure, we keep a tree of nodes that have been 
expanded. The tree is represented as a list of paths.  Also, for each
procedure, a path for the node that shows at the top of the scrolling 
window.
*)

type context = ModCxt of ident | ProcCxt of string | GlobCxt

let expand_tbl = (Hashtbl.create 100 : (context, selector list list) Hashtbl.t)
let scroll_tbl = Hashtbl.create 100

let get_expand x = try Hashtbl.find expand_tbl x with Not_found -> []
let put_expand x t = Hashtbl.add expand_tbl x t

let get_scroll x = Hashtbl.find scroll_tbl x
let put_scroll x p = Hashtbl.add scroll_tbl x p

let rec prefix us vs =
  match (us, vs) with
      ([], _) -> true
    | (x::xs, []) -> false
    | (x::xs, y::ys) -> x = y && prefix xs ys

let add_path p ps = 
  if List.exists (fun q -> prefix p q) ps then ps
  else p :: List.filter (fun q -> not (prefix q p)) ps

let remove_path p ps =
  let n = List.length p in
  let qs = List.filter (fun q -> not (prefix p q)) ps in
  if n = 0 then qs 
  else add_path (Util.take (n-1) p) qs

let remember f x p =
  let remem y q = put_expand y (f q (get_expand y)) in
  match p with
      Module m :: p' -> remem (ModCxt m) p'
    | _ -> remem x p

class type varsview =
  object
    method clear : unit -> unit
    method show_frame : regs -> unit
    method show_globals : unit -> unit
    method as_widget : GObj.widget
  end

let showable d =
  match d.d_kind with VarDef | ParamDef | VParamDef -> true | _ -> false

let rec has_children x v =
  match x with
      (Name _ | Subs _) ->
	let t = type_of v in
	if is_pointer t then
	  not (null_pointer v) && has_children x (deref v)
	else if is_array t then
	  bound t > 0
	else if is_record t then
	  let r = get_record t in r.r_fields <> []
	else
	  false
    | Module m ->
        let env = Info.get_module m in
	List.exists showable (top_block env)
    | Dummy -> failwith "has_children"
    | DotDotDot -> false

let rec do_children x v f =
  match x with
      (Name _ | Subs _) ->
	let t = type_of v in
	if is_pointer t then
	  do_children x (deref v) f
	else if is_array t && not (is_flex t) then begin
	  let show i = f (Subs i) (subscript v i) in
	  let b = bound t in
	  let limit = 100 in
	  if b <= limit then
	    for i = 0 to b-1 do show i done
	  else begin
	    for i = 0 to limit-3 do show i done;
	    f DotDotDot void_value;
	    show (b-1)
	  end
	end
	else if is_record t then
	  let r = get_record t in
	  List.iter (fun d -> f (Name d.d_tag) (select v d)) r.r_fields
	else
	  ()
    | Module m ->
	let env = Info.get_module m in
	do_block Int32.zero (top_block env) f
    | Dummy | DotDotDot -> failwith "do_children"

and do_block bp ds f =
  List.iter (fun d -> 
      if showable d then
	f (Name d.d_tag) (def_value bp d)) ds

class varsview_impl (peer : GTree.view) =
  let cols = new GTree.column_list in
  let c_name = cols#add Gobject.Data.caml in
  let c_value = cols#add Gobject.Data.caml in
  let c_disp = cols#add Gobject.Data.string in
    (* c_disp is a cache of the printed value: this avoids fetching
       data repeatedly if the value is e.g. a string. *)
  let store = GTree.tree_store cols in
  let context = ref GlobCxt in
  let cleared = ref true in

  let vc_name = GTree.view_column ~title:"Name" () in
  let vc_value = GTree.view_column ~title:"Value" 
    ~renderer:(GTree.cell_renderer_text [], ["text", c_disp]) () in
  let vc_type = GTree.view_column ~title:"Type" () in

  let insert ?parent x v =
    let iter = store#append ?parent () in
    store#set ~row:iter ~column:c_name x;
    store#set ~row:iter ~column:c_value v;
    let s = 
      match x with
	  Name _ | Subs _ -> sprintf "$" [fLongVal v]
	| Module _ | Dummy | DotDotDot -> "" in
    store#set ~row:iter ~column:c_disp s;
    if has_children x v then begin
      let dummy = store#append ~parent:iter () in
      store#set ~row:dummy ~column:c_name Dummy;
      store#set ~row:dummy ~column:c_value void_value;
      store#set ~row:dummy ~column:c_disp ""
    end in

  let insert_module ?parent m =
    let env = Info.get_module m in
    if List.exists showable (top_block env) then
      insert ?parent (Module m) void_value in

  let get_children iter =
    List.map (fun i -> store#iter_children ~nth:i iter) 
      (Util.range 0 (store#iter_n_children iter - 1)) in

  object (self)
    method as_widget = (peer :> GObj.widget)

    method show_globals () =
      self#clear ();
      List.iter (fun m ->
	  if not (List.mem m Debconf.lib_mods) then insert_module m)
        (Binary.all_modules ());
      self#expand_tree GlobCxt

    method show_frame regs =
      let p = curr_proc regs.cp in
      let d = Info.get_debug p.s_name in
      if d.d_tag = intern "MAIN" then 
	self#show_globals ()
      else if d.d_tag = intern "*body*" then begin
        self#clear ();
	let env = Info.get_module d.d_module in
	do_block Int32.zero (top_block env) insert;
	self#expand_tree (ModCxt d.d_module)
      end
      else begin
        self#clear ();
	do_block regs.bp (top_block d.d_env) insert;
	insert_module d.d_module;
        self#expand_tree (ProcCxt p.s_name)
      end

    method expand_tree x =
      context := x; cleared := false;
      let rec expand itz =
	function
	    [] -> ()
	  | x::xs ->
	      let kids = get_children itz in	
	      try
		let it' = List.find (fun it ->
		  store#get ~row:it ~column:c_name = x) kids in
		self#populate it';
		peer#expand_row (store#get_path it');
		expand (Some it') xs 
	      with Not_found -> () in
      let expand_mod it =
        match store#get ~row:it ~column:c_name with
	    Module m ->
	      List.iter 
		(fun p -> expand None (Module m :: p)) 
		(get_expand (ModCxt m))
	  | _ -> () in
      List.iter (expand None) (get_expand x);
      List.iter expand_mod (get_children None);
      try
	let iter = self#iter_sel (get_scroll x) in
	peer#scroll_to_cell ~align:(0.0, 0.0) (store#get_path iter) vc_name
      with Not_found -> ()
	
    (* sel_path -- convert tree iterator to selector path *)
    method sel_path iter =
      let rec loop itz xs =
	match itz with
	    None -> xs
	  | Some it ->
	      let x = store#get ~row:it ~column:c_name in
	      loop (store#iter_parent it) (x::xs) in
      loop (Some iter) []

    (* iter_sel -- convert selector path to tree iterator *)
    method iter_sel path =
      let rec loop itz =
	function
	    [] -> itz
	  | x::xs ->
	      let kids = get_children itz in
	      try
		let it' = List.find (fun it ->
		  store#get ~row:it ~column:c_name = x) kids in
		loop (Some it') xs
	      with Not_found -> 
		(* Truncated path *)
		itz in
      match loop None path with
	  Some it -> it
	| None -> raise Not_found

    method populate iter =
      if store#iter_n_children (Some iter) = 1 then begin
	let child = store#iter_children (Some iter) in
	if store#get ~row:child ~column:c_name = Dummy then begin
	  let x = store#get ~row:iter ~column:c_name in
	  let v = store#get ~row:iter ~column:c_value in
	  do_children x v (insert ~parent:iter);
	  ignore (store#remove child)
	end
      end

    method private open_node iter path = 
      remember add_path !context (self#sel_path iter);
      self#populate iter

    method private close_node iter path =
      remember remove_path !context (self#sel_path iter)

    method clear () = 
      if not !cleared then begin
	let p =
	  match peer#get_visible_range () with
	      Some (start, _) -> self#sel_path (store#get_iter start) 
	    | None -> [] in
	put_scroll !context p;
	store#clear ();
	cleared := true
      end

  initializer
    peer#set_model (Some store#coerce);
    peer#misc#modify_font_by_name Debconf.sans_font;

    let render_name = GTree.cell_renderer_text [] in
    vc_name#pack render_name;
    vc_name#set_cell_data_func render_name (fun model row ->
      let x = model#get ~row ~column:c_name in
      let v = model#get ~row ~column:c_value in
      let t = type_of v in
      let s = sprintf "$$" 
	    	[fSel x; if is_pointer t then fStr uparrow else fStr ""] in
      render_name#set_properties [`TEXT s]);

    let render_type = GTree.cell_renderer_text [] in
    vc_type#pack render_type;
    vc_type#set_cell_data_func render_type (fun model row ->
      let v = model#get ~row ~column:c_value in
      let t = type_of v in
      let s = if same_types t voidtype then "" 
		else sprintf "$" [fType t] in
      render_type#set_properties [`TEXT s]);

    ignore (peer#append_column vc_name); vc_name#set_resizable true;
    ignore (peer#append_column vc_value); vc_value#set_resizable true;
    ignore (peer#append_column vc_type); vc_type#set_resizable true;

    ignore (peer#connect#row_expanded ~callback:self#open_node);
    ignore (peer#connect#row_collapsed ~callback:self#close_node)
  end

let varsview ?width ?height ?packing () =
  (new varsview_impl (GTree.view ?width ?height ?packing ()) :> varsview)
