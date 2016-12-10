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

/* $Id: ml_gdk.c 1452 2009-05-08 10:15:38Z garrigue $ */

#include <string.h>
#include <gdk/gdk.h>
#if defined(_WIN32) || defined(__MINGW32__)
#include <gdk/gdkwin32.h>
#else
#if defined(HAS_GTKQUARTZ)
#else
#include <gdk/gdkx.h>
#endif
#endif
#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/callback.h>
#include <caml/bigarray.h>

#include "wrappers.h"
#include "ml_gpointer.h"
#include "ml_glib.h"
#include "ml_gobject.h"
#include "ml_pango.h"
#include "ml_gdk.h"
#include "ml_gdkpixbuf.h"
#include "gdk_tags.h"


#ifndef HASGTK22
#define GDK_WINDOW_TYPE_HINT_SPLASHSCREEN GDK_WINDOW_TYPE_HINT_NORMAL
#define GDK_WINDOW_TYPE_HINT_DESKTOP GDK_WINDOW_TYPE_HINT_NORMAL
#define GDK_WINDOW_TYPE_HINT_UTILITY GDK_WINDOW_TYPE_HINT_NORMAL
#define GDK_WINDOW_TYPE_HINT_DOCK GDK_WINDOW_TYPE_HINT_NORMAL
#endif

CAMLprim void ml_raise_gdk (const char *errmsg)
{
  static value * exn = NULL;
  if (exn == NULL)
      exn = caml_named_value ("gdkerror");
  raise_with_string (*exn, (char*)errmsg);
}

CAMLprim value ml_gdk_init(value unit)
{
  /* Since these are declared const, must force gcc to call them! */
  GType t =
    gdk_color_get_type();
  return Val_GType(t);
}

#include "gdk_tags.c"

Make_OptFlags_val (GdkModifier_val)
Make_Flags_val (GdkModifier_val)
Make_Flags_val (Event_mask_val)

#include <stdio.h>

#define Make_test(conv) \
CAMLprim value ml_test_##conv (value mask, value test) \
{ return Val_bool (conv(mask) & Int_val(test)); }

Make_test(GdkModifier_val)
Make_test(GdkWindowState_val)

/* Colormap */

ML_0 (gdk_colormap_get_system, Val_GdkColormap)


/* Color */

ML_2 (gdk_colormap_new, GdkVisual_val, Bool_val, Val_GdkColormap)
ML_1 (gdk_colormap_get_visual, GdkColormap_val, Val_GdkVisual)

CAMLprim value ml_gdk_color_white (value cmap)
{
    GdkColor color;
    gdk_color_white (GdkColormap_val(cmap), &color);
    return Val_copy(color);
}
    
CAMLprim value ml_gdk_color_black (value cmap)
{
    GdkColor color;
    gdk_color_black (GdkColormap_val(cmap), &color);
    return Val_copy(color);
}

CAMLprim value ml_gdk_color_parse (char *spec)
{
    GdkColor color;
    if (!gdk_color_parse (spec, &color))
        ml_raise_gdk ("color_parse");
    return Val_copy(color);
}

ML_2 (gdk_color_alloc, GdkColormap_val, GdkColor_val, Val_bool)

CAMLprim value ml_GdkColor (value red, value green, value blue)
{
    GdkColor color;
    color.red = Int_val(red);
    color.green = Int_val(green);
    color.blue = Int_val(blue);
    color.pixel = 0;
    return Val_copy(color);
}

Make_Extractor (GdkColor, GdkColor_val, red, Val_int)
Make_Extractor (GdkColor, GdkColor_val, green, Val_int)
Make_Extractor (GdkColor, GdkColor_val, blue, Val_int)
Make_Extractor (GdkColor, GdkColor_val, pixel, Val_int)

/* Rectangle */

CAMLprim value ml_GdkRectangle (value x, value y, value width, value height)
{
    GdkRectangle rectangle;
    rectangle.x = Int_val(x);
    rectangle.y = Int_val(y);
    rectangle.width = Int_val(width);
    rectangle.height = Int_val(height);
    return Val_copy(rectangle);
}

Make_Extractor (GdkRectangle, GdkRectangle_val, x, Val_int)
Make_Extractor (GdkRectangle, GdkRectangle_val, y, Val_int)
Make_Extractor (GdkRectangle, GdkRectangle_val, width, Val_int)
Make_Extractor (GdkRectangle, GdkRectangle_val, height, Val_int)

/* Pixmap */

CAMLexport GdkPixmap *GdkPixmap_val(value val)
{
    if (!Field(val,1)) ml_raise_gdk ("attempt to use destroyed GdkPixmap");
    return check_cast(GDK_PIXMAP,val);
}

/* Properties */

ML_2 (gdk_atom_intern, String_val, Int_val, Val_GdkAtom)
ML_1 (gdk_atom_name, GdkAtom_val, Val_string)

CAMLprim value ml_gdk_property_change (value window, value property, value type,
                              value mode, value xdata)
{
    int format = Xdata_val (Field(xdata,0));
    value data = Field(xdata,1);
    int nelems = (format == 8 ? string_length (data) : Wosize_val(data));
    guchar *sdata;
    int i;
    switch (format) {
    case 16:
        sdata = calloc(nelems, sizeof(short)); 
        for (i=0; i<nelems; i++)
            ((gushort*)sdata)[i] = Int_val(Field(data,i));
        break;
    case 32:
        sdata = calloc(nelems, sizeof(long));
        for (i=0; i<nelems; i++)
            ((gulong*)sdata)[i] = Int32_val(Field(data,i)); 
        break;
    default:
        sdata = (guchar*)data;
    }
    gdk_property_change (GdkWindow_val(window), GdkAtom_val(property),
                         GdkAtom_val(type), format, Property_mode_val(mode),
                         sdata, nelems);
    if (format != 8) free(sdata);
    return Val_unit;
}

/* copy X11 property data */
CAMLprim value copy_xdata (gint format, void *xdata, gulong nitems)
{
    CAMLparam0();
    CAMLlocal1(data);
    value ret = MLTAG_NONE;
    value tag;
    unsigned int i;
    switch (format) {
    case 8:
        data = alloc_string (nitems);
        memcpy (String_val(data), xdata, sizeof(char) * nitems);
        tag = MLTAG_BYTES;
        break;
    case 16:
        data = alloc (nitems,0);
        for (i = 0; i < nitems; i++)
            Field(data,i) = Val_int(((short*)xdata)[i]);
        tag = MLTAG_SHORTS;
        break;
    case 32:
        data = alloc (nitems,0);
        for (i = 0; i < nitems; i++)
            Store_field(data, i, copy_int32 (((long*)xdata)[i]));
        tag = MLTAG_INT32S;
        break;
    default:
        tag = MLTAG_NONE;
    }
    if (tag != MLTAG_NONE) {
        ret = alloc_small (2,0);
        Field(ret,0) = tag;
        Field(ret,1) = data;
    }
    CAMLreturn(ret);
}

CAMLprim value ml_gdk_property_get (value window, value property,
                           value length, value pdelete)
{
#if defined(_WIN32) || defined(__CYGWIN__) || defined(HAS_GTKQUARTZ)
  return Val_unit; /* not supported */
#else
    GdkAtom atype;
    int aformat, alength;
    guchar *data;
    int nitems;
    int ok = gdk_property_get (GdkWindow_val(window), GdkAtom_val(property),
                               AnyPropertyType, 0,
                               Long_val(length), Bool_val(pdelete),
                               &atype, &aformat, &alength, &data);
    if (ok) {
        CAMLparam0();
        CAMLlocal3(mltype, mldata, pair);
        switch (aformat) {
        case 16: nitems = alength / sizeof(short); break;
        case 32: nitems = alength / sizeof(long); break;
        default: nitems = alength;
        }
        mldata = copy_xdata (aformat, data, nitems);
        mltype = Val_GdkAtom (atype);
        pair = alloc_small(2,0);
        Field(pair,0) = mltype;
        Field(pair,1) = mldata;
        CAMLreturn(ml_some (pair));
    }
    return Val_unit;
#endif
}

ML_2 (gdk_property_delete, GdkWindow_val, GdkAtom_val, Unit)

/* Region */

#define PointArray_val(val) ((GdkPoint*)&Field(val,1))
#define PointArrayLen_val(val) Int_val(Field(val,0))
Make_Val_final_pointer (GdkRegion, Ignore, gdk_region_destroy, 0)
#define Val_GdkRegion_copy(r) (Val_GdkRegion(gdk_region_copy(r)))
CAMLexport GdkRegion *GdkRegion_val(value val)
{
    if (!Field(val,1)) ml_raise_gdk ("attempt to use destroyed GdkRegion");
    return (GdkRegion*)(Field(val,1));
}
CAMLprim value ml_gdk_region_destroy (value val)
{
    if (Field(val,1)) gdk_region_destroy((GdkRegion*)(Field(val,1)));
    Field(val,1) = 0;
    return Val_unit;
}
ML_0 (gdk_region_new, Val_GdkRegion)
ML_2 (gdk_region_polygon, Insert(PointArray_val(arg1)) PointArrayLen_val,
      Fill_rule_val, Val_GdkRegion)
ML_1 (gdk_region_copy, GdkRegion_val, Val_GdkRegion)
ML_2 (gdk_region_intersect, GdkRegion_val, GdkRegion_val, Unit)
ML_2 (gdk_region_union, GdkRegion_val, GdkRegion_val, Unit)
ML_2 (gdk_region_subtract, GdkRegion_val, GdkRegion_val, Unit)
ML_2 (gdk_region_xor, GdkRegion_val, GdkRegion_val, Unit)
ML_2 (gdk_region_union_with_rect, GdkRegion_val, GdkRectangle_val, Unit)
ML_3 (gdk_region_offset, GdkRegion_val, Int_val, Int_val, Unit)
ML_3 (gdk_region_shrink, GdkRegion_val, Int_val, Int_val, Unit)
ML_1 (gdk_region_empty, GdkRegion_val, Val_bool)
ML_2 (gdk_region_equal, GdkRegion_val, GdkRegion_val, Val_bool)
ML_3 (gdk_region_point_in, GdkRegion_val, Int_val, Int_val, Val_bool)
ML_2 (gdk_region_rect_in, GdkRegion_val, GdkRectangle_val, Val_overlap_type)
ML_2 (gdk_region_get_clipbox, GdkRegion_val, GdkRectangle_val, Unit)


/* Events */

/* Have a major collection every 1000 events */
Make_Val_final_pointer (GdkEvent, Ignore, gdk_event_free, 1)
ML_1 (gdk_event_copy, GdkEvent_val, Val_GdkEvent)

#ifdef HASGTK22
CAMLprim value ml_gdk_event_new (value event_type)
{
    GdkEvent *event = gdk_event_new(Event_type_val(event_type));
    event->any.send_event = TRUE;
    return Val_GdkEvent(event);
}
#else
CAMLprim value ml_gdk_event_new (value event_type)
{
    GdkEvent event;
    memset (&event, 0, sizeof(GdkEvent));
    event.type = Event_type_val(event_type);
    event.any.send_event = TRUE;
    return Val_copy(event);
}
#endif

ML_1 (gdk_event_get_time, GdkEvent_val, copy_int32)

#define GdkEvent_arg(type) (GdkEvent##type*)GdkEvent_val

Make_Extractor (GdkEventAny, GdkEvent_arg(Any), type, Val_event_type)
Make_Extractor (GdkEventAny, GdkEvent_arg(Any), window, Val_GdkWindow)
Make_Extractor (GdkEventAny, GdkEvent_arg(Any), send_event, Val_bool)
Make_Setter (gdk_event_set, GdkEvent_arg(Any), Event_type_val, type)
Make_Setter (gdk_event_set, GdkEvent_arg(Any), GdkWindow_val, window)

Make_Extractor (GdkEventExpose, GdkEvent_arg(Expose), area, Val_copy)
Make_Extractor (GdkEventExpose, GdkEvent_arg(Expose), region,
                Val_GdkRegion_copy)
Make_Extractor (GdkEventExpose, GdkEvent_arg(Expose), count, Val_int)

Make_Extractor (GdkEventVisibility, GdkEvent_arg(Visibility), state,
                Val_gdkVisibilityState)

Make_Extractor (GdkEventMotion, GdkEvent_arg(Motion), x, copy_double)
Make_Extractor (GdkEventMotion, GdkEvent_arg(Motion), y, copy_double)
static value copy_axes(double *axes)
{
    CAMLparam0();
    CAMLlocal2(x,y);
    value ret;
    if (axes) {
        x = copy_double(axes[0]);
        y = copy_double(axes[0]);
        ret = alloc_small(2, 0);
        Field(ret,0) = x;
        Field(ret,1) = y;
        ret = ml_some(ret);
    }
    else ret = Val_unit;
    CAMLreturn(ret);
}
Make_Extractor (GdkEventMotion, GdkEvent_arg(Motion), axes, copy_axes)
Make_Extractor (GdkEventMotion, GdkEvent_arg(Motion), state, Val_int)
Make_Extractor (GdkEventMotion, GdkEvent_arg(Motion), is_hint, Val_int)
Make_Extractor (GdkEventMotion, GdkEvent_arg(Motion), device, Val_GdkDevice)
Make_Extractor (GdkEventMotion, GdkEvent_arg(Motion), x_root, copy_double)
Make_Extractor (GdkEventMotion, GdkEvent_arg(Motion), y_root, copy_double)

Make_Extractor (GdkEventButton, GdkEvent_arg(Button), x, copy_double)
Make_Extractor (GdkEventButton, GdkEvent_arg(Button), y, copy_double)
Make_Extractor (GdkEventButton, GdkEvent_arg(Button), axes, copy_axes)
Make_Extractor (GdkEventButton, GdkEvent_arg(Button), state, Val_int)
Make_Extractor (GdkEventButton, GdkEvent_arg(Button), button, Val_int)
Make_Extractor (GdkEventButton, GdkEvent_arg(Button), device, Val_GdkDevice)
Make_Extractor (GdkEventButton, GdkEvent_arg(Button), x_root, copy_double)
Make_Extractor (GdkEventButton, GdkEvent_arg(Button), y_root, copy_double)
Make_Setter (gdk_event_button_set, GdkEvent_arg(Button), Int_val, button)

Make_Extractor (GdkEventScroll, GdkEvent_arg(Scroll), x, copy_double)
Make_Extractor (GdkEventScroll, GdkEvent_arg(Scroll), y, copy_double)
Make_Extractor (GdkEventScroll, GdkEvent_arg(Scroll), state, Val_int)
Make_Extractor (GdkEventScroll, GdkEvent_arg(Scroll),
                direction, Val_gdkScrollDirection)
Make_Extractor (GdkEventScroll, GdkEvent_arg(Scroll), device, Val_GdkDevice)
Make_Extractor (GdkEventScroll, GdkEvent_arg(Scroll), x_root, copy_double)
Make_Extractor (GdkEventScroll, GdkEvent_arg(Scroll), y_root, copy_double)

Make_Extractor (GdkEventKey, GdkEvent_arg(Key), state, Val_int)
Make_Extractor (GdkEventKey, GdkEvent_arg(Key), keyval, Val_int)
Make_Extractor (GdkEventKey, GdkEvent_arg(Key), string, Val_string)
Make_Extractor (GdkEventKey, GdkEvent_arg(Key), hardware_keycode, Val_int)
Make_Extractor (GdkEventKey, GdkEvent_arg(Key), group, Val_int)

Make_Extractor (GdkEventCrossing, GdkEvent_arg(Crossing),
                subwindow, Val_GdkWindow)
Make_Extractor (GdkEventCrossing, GdkEvent_arg(Crossing), x, copy_double)
Make_Extractor (GdkEventCrossing, GdkEvent_arg(Crossing), y, copy_double)
Make_Extractor (GdkEventCrossing, GdkEvent_arg(Crossing), x_root, copy_double)
Make_Extractor (GdkEventCrossing, GdkEvent_arg(Crossing), y_root, copy_double)
Make_Extractor (GdkEventCrossing, GdkEvent_arg(Crossing),
                mode, Val_gdkCrossingMode)
Make_Extractor (GdkEventCrossing, GdkEvent_arg(Crossing),
                detail, Val_gdkNotifyType)
Make_Extractor (GdkEventCrossing, GdkEvent_arg(Crossing), focus, Val_bool)
Make_Extractor (GdkEventCrossing, GdkEvent_arg(Crossing), state, Val_int)

Make_Extractor (GdkEventFocus, GdkEvent_arg(Focus), in, Val_int)

Make_Extractor (GdkEventConfigure, GdkEvent_arg(Configure), x, Val_int)
Make_Extractor (GdkEventConfigure, GdkEvent_arg(Configure), y, Val_int)
Make_Extractor (GdkEventConfigure, GdkEvent_arg(Configure), width, Val_int)
Make_Extractor (GdkEventConfigure, GdkEvent_arg(Configure), height, Val_int)

Make_Extractor (GdkEventProperty, GdkEvent_arg(Property), atom, Val_GdkAtom)
Make_Extractor (GdkEventProperty, GdkEvent_arg(Property), state, Val_int)

Make_Extractor (GdkEventSelection, GdkEvent_arg(Selection), selection,
                Val_GdkAtom)
Make_Extractor (GdkEventSelection, GdkEvent_arg(Selection), target,
                Val_GdkAtom)
Make_Extractor (GdkEventSelection, GdkEvent_arg(Selection), property,
                Val_GdkAtom)

Make_Extractor (GdkEventProximity, GdkEvent_arg(Proximity),
                device, Val_GdkDevice)

Make_Extractor (GdkEventClient, GdkEvent_arg(Client), window, Val_GdkWindow)
Make_Extractor(GdkEventClient, GdkEvent_arg(Client), message_type, Val_GdkAtom)
CAMLprim value ml_GdkEventClient_data (GdkEventClient *ev)
{
    int nitems = 0;
    switch (ev->data_format) {
    case 8:  nitems = 20; break;
    case 16: nitems = 10; break;
    case 32: nitems = 5;  break;
    }
    return copy_xdata (ev->data_format, ev->data.b, nitems);
}

Make_Extractor (GdkEventSetting, GdkEvent_arg(Setting),
                action, Val_gdkSettingAction)
Make_Extractor (GdkEventSetting, GdkEvent_arg(Setting), name, copy_string)

Make_Extractor (GdkEventWindowState, GdkEvent_arg(WindowState),
                changed_mask, Val_int)
Make_Extractor (GdkEventWindowState, GdkEvent_arg(WindowState),
                new_window_state, Val_int)

