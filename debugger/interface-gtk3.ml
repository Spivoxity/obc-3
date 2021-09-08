module GSourceView = GSourceView3

external rgba_parse : string -> Gdk.rgba = "ml_rgba_parse"

let define_category (peer : GSourceView3.source_view) name icon prio bgrd =
  let attr = GSourceView3.source_mark_attributes () in
  attr#set_pixbuf (Debconf.pixbuf_resource icon);
  if bgrd <> "" then
    attr#set_background (rgba_parse bgrd);
  peer#set_mark_attributes name attr prio
