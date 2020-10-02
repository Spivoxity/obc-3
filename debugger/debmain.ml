(*
 * debmain.ml
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

open Print
open Binary
open Symtab
open Control

let args = ref []

(* Main window *)

let ask_ok action =
  match !Control.state with
      Ready | Exited | Error -> true
    | _ ->
	GToolbox.question_box 
	  ~title:(sprintf "Really $?" [fStr action])
	  ~buttons:["No"; "Yes"] ~default:2 
	  (sprintf "The program is running.\nReally $?" [fStr action])
	= 2

class main_window () =
  let peer = GWindow.window () in 
  let contents = GPack.vbox () in
  let menubar = GMenu.menu_bar ~packing:contents#pack () in
  let toolbar = GButton.toolbar ~style:`BOTH ~packing:contents#pack () in
  let hpaned = 
    GPack.paned `HORIZONTAL ~packing:(contents#pack ~expand:true) () in
  let vpaned2 = 
    GPack.paned `VERTICAL ~packing:(hpaned#pack2 ~resize:false) () in
  let notebook = 
    Sourcebook.sourcebook ~packing:(hpaned#pack1 ~resize:true) () in
  let varsframe = 
    GBin.frame ~label:"Variables" ~packing:(vpaned2#pack2 ~resize:true) () in
  let varsscroll = GBin.scrolled_window
      ~hpolicy:`AUTOMATIC ~vpolicy:`AUTOMATIC ~packing:varsframe#add () in
  let varsview = Varsview.varsview ~packing:varsscroll#add () in
  let stackframe = GBin.frame ~label:"Stack" ~packing:vpaned2#add1 () 
      ~width:300 ~height:180 in
  let stackscroll = GBin.scrolled_window
      ~hpolicy:`AUTOMATIC ~vpolicy:`AUTOMATIC ~packing:stackframe#add () in
  let stackview = Stackview.stackview notebook varsview 
      ~packing:stackscroll#add () in
  let status = GMisc.statusbar ~packing:contents#pack () in

  let statcxt = status#new_context "Status" in

  let toolbutton text tooltip ifile =
    let icon = Debconf.image_resource ifile in
    toolbar#insert_button ~text ~tooltip ~icon:icon#coerce () in

  let runbtn = 
    toolbutton "Run" "Start or continue the program" "continue.png" in
  let pausebtn = 
    toolbutton "Pause" "Pause the program" "pause.png" in
  let stepintobtn = 
    toolbutton "Step into" "Run for one step" "stepinto.png" in
  let stepoverbtn =
    toolbutton "Step over" "Run to next line" "stepover.png" in
  let stepoutbtn =
    toolbutton "Step out" "Run until procedure returns" "stepout.png" in
  let restartbtn =
    toolbutton "Restart" "Restart the program" "restart.png" in
  let exitbtn =
    toolbutton "Exit" "Quit the debugger" "exit.png" in

  let modmenu = GMenu.menu () in
  let modcount = ref 0 in

  let mutex = Mutex.create () in

  let about_dialog = ref (fun () -> raise Not_found) in

  object (self)
    method report_status fmt args =
      statcxt#pop (); ignore (statcxt#push (sprintf fmt args))

    method set_button_states b =
      (* On some platforms -- Windows included -- hovering over a button
         which is changing from insensitive to sensitive doesn't make
	 it possible to click.  This is annoying, so we do not make
	 the action buttons insensitive when the program runs. *)
      let c = (!Control.state <> Exited && !Control.state <> Error) in
      List.iter (fun btn -> btn#misc#set_sensitive c)
	[runbtn; stepintobtn; stepoverbtn; stepoutbtn];
      pausebtn#misc#set_sensitive b;

    method print_loc msg =
      match !location with
	  Line (m, p, n) ->
	    self#report_status 
	      "$ at module $, line $" [fStr msg; fId m; fNum n];
	| Proc p ->
	    self#report_status "$ in procedure $" [fStr msg; fStr p]
	| NoLoc ->
	    self#report_status "$" [fStr msg]

    method run cmd =
      (* Ignore clicks when the button should not be enabled *)
      if !Control.state <> Running && !Control.state <> Exited 
	  && !Control.state <> Error then begin
	self#report_status "Running" [];
	self#set_button_states true;
	notebook#unmark true;
	notebook#set_click_enabled false;
	stackview#clear ();
	varsview#clear ();
	ignore (Thread.create (fun () ->
	  Mutex.lock mutex;
	  cmd ();
	  Mutex.unlock mutex;
	  GtkThread.async (fun () ->
	    Mutex.lock mutex;
	    self#stopped (); 
	    Mutex.unlock mutex) ()) ())
      end

    method pause () =
      Debconf.flash_message "Interrupted!" [];
      interrupt ()

    method restart () = 
      if ask_ok "restart" then begin
	interrupt ();
	Mutex.lock mutex;
	Procio.close ();
	Binary.clear ();

	location := NoLoc;
	notebook#unmark true;
	notebook#set_click_enabled false;
	stackview#clear ();
	varsview#clear ();
	
	start_program !args;

	Mutex.unlock mutex;
	List.iter (fun (m, n) -> Control.set_break m n true)
	  (notebook#all_breakpoints ());
	self#stopped ()
      end

    method quit () =
      if ask_ok "exit" then begin
	interrupt ();
	GMain.quit ()
      end

    method stopped () =
      if !Control.state = Exited then begin
	self#report_status "The program has exited" [];
	varsview#show_globals ()
      end
      else begin
	self#print_loc (Control.state_name ());
	notebook#goto_loc !location true;
	stackview#backtrace ();
	if !Control.state = Error then begin
	  let dialog = 
	    GWindow.message_dialog ~title:"Runtime error"
	      ~message_type:`ERROR ~buttons:GWindow.Buttons.ok 
	      ~message:("The program has halted with a runtime error:\n" 
		^ !err_message) 
	      ~show:true () in
	  ignore (dialog#run ());
	  dialog#destroy ();
	  Control.state := Exited
	end
      end;
      self#set_button_states false;
      notebook#set_click_enabled true;

    (* Add a module to the menu *)
    method add_module m =
      try
	let fname = Info.module_source m in
	incr modcount;
	let format = if !modcount < 10 then "_$ $" else "$ $" in
	let item = 
	  GMenu.menu_item 
	    ~label:(sprintf format [fNum !modcount; fId m]) 
	    ~use_mnemonic:true ~packing:modmenu#append () in
	ignore (item#connect#activate (fun ev -> notebook#show_module m));
	ignore (notebook#load_file m fname)
      with Not_found -> ()

    method about () =
      try 
	!about_dialog ()
      with Not_found ->
	let dialog = GWindow.about_dialog ~version:Config.version
	  ~comments:(sprintf "Debugging monitor version $" 
				[fStr !monitor_version]) () in
	ignore (dialog#connect#close 
	  ~callback:(fun ev -> dialog#misc#hide ()));
	ignore (dialog#connect#response
	  ~callback:(fun btn -> dialog#misc#hide ()));
	about_dialog := dialog#present;
	dialog#show ()

    method show () = peer#show ()

  initializer  
    peer#set_title "Oberon Debugger";
    peer#misc#set_size_request ~height:200 ~width:200 ();
    peer#set_default_size ~height:600 ~width:900;
    peer#add contents#coerce;

    let flash_cxt = status#new_context "Flash" in
    Debconf.flash_msg := (fun msg -> flash_cxt#flash msg);

    List.iter (fun (btn, action) -> 
	ignore (btn#connect#clicked ~callback:(fun ev -> self#run action)))
      [runbtn, run; stepintobtn, step_into; 
	stepoverbtn, step_over; stepoutbtn, step_out];
    ignore (pausebtn#connect#clicked ~callback:(fun ev -> self#pause ()));
    ignore (restartbtn#connect#clicked ~callback:(fun ev -> self#restart ()));
    ignore (exitbtn#connect#clicked ~callback:(fun ev -> self#quit ()));

    let make_menu name entries =
      let item = GMenu.menu_item ~label:name ~use_mnemonic:true
	~packing:menubar#append () in
      let menu = GMenu.menu ~packing:item#set_submenu () in
      GToolbox.build_menu menu ~entries:entries in

    if not Debconf.macos then
      make_menu "_File" [`I ("_Quit", self#quit)];

    let moditem = 
      GMenu.menu_item ~label:"_Modules" ~use_mnemonic:true
        ~packing:menubar#append () in
    moditem#set_submenu modmenu;

    make_menu "_Options" 
      [`C ("_Uglify syntax", notebook#uglify, notebook#set_uglify)];

    if not Debconf.macos then
      make_menu "_Help" [`I ("_About Obdb", self#about)]
    else begin
      let about_item = GMenu.menu_item ~label:"About Obdb" () in
      let _ = about_item#connect#activate self#about in
      GMain.set_platform_menubar menubar about_item
    end;

    ignore (peer#event#connect#delete 
      ~callback:(fun ev -> self#quit (); true));
  end

let print_version f =
  fprintf f "Oxford Oberon-2 gui debugger version $ [build $]\n" 
    [fStr Config.version; fStr Revid.hash]

let anon_fun s = args := !args @ [s]

let spec =
  [ "-d", Arg.Set Procio.trace, "Trace process I/O";
    "-i", Arg.String (fun s -> Procio.interp := s), "Specify interpreter";
    "-I", Arg.String (fun s -> Config.libpath := !Config.libpath @ [s]), 
      "Search directory for imports";
    "-R", Arg.String (fun s -> Debconf.resource_dir := s),
      "Directory for gui resources";
    "-v", Arg.Unit (fun () -> print_version stderr; exit 0), 
		"Print version and exit";
    "--", Arg.Rest anon_fun, "Following arguments are for program" ]

let main () = 
  Arg.parse spec anon_fun "Usage:";

  ignore (GMain.init ());
  Glib.set_application_name "Oxford Oberon-2 debugger";

  (let mgr = GSourceView2.source_language_manager ~default:true in
    mgr#set_search_path (!Debconf.resource_dir :: mgr#search_path));

  (let mgr = GSourceView2.source_style_scheme_manager ~default:true in
    mgr#set_search_path (!Debconf.resource_dir :: mgr#search_path));

  GtkMain.Rc.parse_string 
    "style \"tab-close-button-style\" {
        GtkWidget::focus-padding = 0
        GtkWidget::focus-line-width = 0
        xthickness = 0
        ythickness = 0
      }
      widget \"*.tab-close-button\" style \"tab-close-button-style\"
      gtk-show-input-method-menu = 0
      gtk-show-unicode-menu = 0";

  Sys.set_signal Sys.sigint Sys.Signal_ignore;

  start_program !args;

  let gui = new main_window () in

  (* Load symbol tables and source code *)
  List.iter (fun m ->
      Info.import m (Binary.checksum m);
      if not (List.mem m Debconf.lib_mods) then gui#add_module m)
    (Binary.all_modules ());
  flush stderr;

  (* Enter the main loop *)
  gui#show ();
  gui#stopped ();
  GtkThread.main ();
  Procio.close ()

let dialog = Unix.handle_unix_error main ()
