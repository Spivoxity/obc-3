/**************************************************************************/
/*                Lablgtk                                                 */
/*                                                                        */
/*    This program is free software; you can redistribute it              */
/*    and/or modify it under the terms of the GNU Library General         */
/*    Public License as published by the Free Software Foundation         */
/*    version 2, with the exception described in file COPYING which       */
/*    comes with the library.                                             */
/*                                                                        */
/*    This program is distributed in the hope that it will be useful,     */
/*    but WITHOUT ANY WARRANTY; without even the implied warranty of      */
/*    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the       */
/*    GNU Library General Public License for more details.                */
/*                                                                        */
/*    You should have received a copy of the GNU Library General          */
/*    Public License along with this program; if not, write to the        */
/*    Free Software Foundation, Inc., 59 Temple Place, Suite 330,         */
/*    Boston, MA 02111-1307  USA                                          */
/*                                                                        */
/*                                                                        */
/**************************************************************************/

/* $Id: ml_gtkmisc.c 1527 2010-09-09 08:02:22Z garrigue $ */

#include <gtk/gtk.h>
#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/callback.h>
#include <caml/fail.h>

#include "wrappers.h"
#include "ml_glib.h"
#include "ml_gobject.h"
#include "ml_gdk.h"
#include "ml_gdkpixbuf.h"
#include "ml_gtk.h"
#include "gtk_tags.h"
#include "gdk_tags.h"

/* Init all */

CAMLprim value ml_gtkmisc_init(value unit)
{
    /* Since these are declared const, must force gcc to call them! */
    GType t =
        gtk_statusbar_get_type() +
        gtk_misc_get_type() +
        gtk_image_get_type() +
        gtk_label_get_type() +
        gtk_tips_query_get_type() +
        gtk_pixmap_get_type() +
        gtk_hseparator_get_type() +
        gtk_vseparator_get_type() +
        gtk_preview_get_type () +
        gtk_font_selection_get_type() +
        gtk_color_selection_get_type();
    return Val_GType(t);
}

/* gtkstatusbar.h */

#define GtkStatusbar_val(val) check_cast(GTK_STATUSBAR,val)
ML_2 (gtk_statusbar_get_context_id, GtkStatusbar_val, String_val, Val_int)
ML_3 (gtk_statusbar_push, GtkStatusbar_val, Int_val, String_val, Val_int)
ML_2 (gtk_statusbar_pop, GtkStatusbar_val, Int_val, Unit)
ML_3 (gtk_statusbar_remove, GtkStatusbar_val, Int_val, Int_val, Unit)
ML_1 (gtk_statusbar_get_has_resize_grip, GtkStatusbar_val, Val_bool)
ML_2 (gtk_statusbar_set_has_resize_grip, GtkStatusbar_val, Bool_val, Unit)

/* gtkimage.h */
#define GtkImage_val(val) check_cast(GTK_IMAGE,val)

#ifdef HASGTK28
ML_1(gtk_image_clear, GtkImage_val, Unit)
#else
Unsupported_28(gtk_image_clear)
#endif

/* gtklabel.h */

#define GtkLabel_val(val) check_cast(GTK_LABEL,val)
ML_2 (gtk_label_set_text, GtkLabel_val, String_val, Unit)
ML_1 (gtk_label_get_text, GtkLabel_val, Val_string)
ML_3 (gtk_label_select_region, GtkLabel_val, Int_val, Int_val, Unit)
CAMLprim value ml_gtk_label_get_selection_bounds (value label)
{
  gint s, e;
  value r;
  if (gtk_label_get_selection_bounds (GtkLabel_val(label), &s, &e)) {
    r = alloc_small(2, 0);
    Field(r, 0) = Val_int(s);
    Field(r, 1) = Val_int(e);
    r = ml_some(r);
  }
  else
    r = Val_unit;
  return r;
}

/* gtktipsquery.h */

#define GtkTipsQuery_val(val) check_cast(GTK_TIPS_QUERY,val)
ML_1 (gtk_tips_query_start_query, GtkTipsQuery_val, Unit)
ML_1 (gtk_tips_query_stop_query, GtkTipsQuery_val, Unit)

/* gtkpixmap.h */

/* gtk[hv]separator.h */
