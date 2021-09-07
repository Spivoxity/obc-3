module GSourceView = GSourceView2

let define_category (peer : GSourceView2.source_view) name icon prio bgrd =
  peer#set_mark_category_pixbuf name
    (Some (Debconf.pixbuf_resource icon));
  peer#set_mark_category_priority name prio;
  if bgrd <> "" then
    peer#set_mark_category_background name
      (Some (GDraw.color (`NAME bgrd)))
