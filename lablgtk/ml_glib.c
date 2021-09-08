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

/* $Id: ml_glib.c 1445 2009-02-09 17:13:00Z ben_99_9 $ */

#include <string.h>
#include <locale.h>
#ifdef _WIN32
#include "win32.h"
#include <wtypes.h>
#include <io.h>
#include <fcntl.h>
#endif
#include <glib.h>
#include <gtk/gtk.h>
#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/callback.h>

#include "wrappers.h"
#include "ml_glib.h"
#include "glib_tags.h"

#include "glib_tags.c"

CAMLprim value ml_glib_init(value unit)
{
  ml_register_exn_map (G_CONVERT_ERROR,
		       "g_convert_error");
  ml_register_exn_map (G_MARKUP_ERROR,
		       "g_markup_error");
  return Val_unit;
}

/* Not from glib! */
ML_2(setlocale, Locale_category_val, String_option_val, Val_optstring)

/* Utility functions */
value copy_string_v (const gchar * const *v)
{
  CAMLparam0();
  CAMLlocal4(h,p,c,s);
  h = p = Val_emptylist;
  while (*v != NULL)
    {
      s = copy_string (*v);
      c = alloc_small (2, 0);
      Field (c, 0) = s;
      Field (c, 1) = Val_emptylist;
      if (p == Val_emptylist)
	h = c;
      else
	Store_field(p, 1, c);
      p = c;
      v++;
    }
  CAMLreturn(h);
}

CAMLprim value copy_string_g_free (char *str)
{
    value res = copy_string_check (str);
    g_free (str);
    return res;
}

static void ml_raise_glib (const char *errmsg)
{
  static const value * exn = NULL;
  if (exn == NULL)
      exn = caml_named_value ("gerror");
  raise_with_string (*exn, (char*)errmsg);
}

CAMLprim value Val_GList (GList *list, value (*func)(gpointer))
{
  CAMLparam0 ();
  CAMLlocal4 (new_cell, result, last_cell, cell);

  last_cell = cell = Val_unit;
  while (list != NULL) {
    result = func(list->data);
    new_cell = alloc_small(2,0);
    Field(new_cell,0) = result;
    Field(new_cell,1) = Val_unit;
    if (last_cell == Val_unit) cell = new_cell;
    else modify(&Field(last_cell,1), new_cell);
    last_cell = new_cell;
    list = list->next;
  }
  CAMLreturn (cell);
}

CAMLprim value Val_GList_free (GList *list, value (*func)(gpointer))
{
  value res = Val_GList (list, func);
  g_list_free (list);
  return res;
}

CAMLprim GList *GList_val (value list, gpointer (*func)(value))
{
    GList *res = NULL;
    for (; Is_block(list); list = Field(list,1))
      res = g_list_append (res, func(Field(list,0)));
    return (res);
}

/* Error handling */
static GSList *exn_map;

struct exn_data {
  GQuark domain;
  char *caml_exn_name;
  value *caml_exn;
};

CAMLprim void ml_register_exn_map (GQuark domain, char *caml_name)
{
  struct exn_data *exn_data = stat_alloc (sizeof *exn_data);
  exn_data->domain = domain;
  exn_data->caml_exn_name = caml_name;
  exn_data->caml_exn = NULL;
  exn_map = g_slist_prepend (exn_map, exn_data);
}

static value *lookup_exn_map (GQuark domain)
{
  GSList *l = exn_map;
  struct exn_data *exn_data;
  for (l = exn_map; l; l=l->next) {
    exn_data = l->data;
    if (exn_data->domain == domain) {
      if (exn_data->caml_exn == NULL)
           exn_data->caml_exn = (value *)caml_named_value (exn_data->caml_exn_name);
      return exn_data->caml_exn;
    }
  }
  return NULL;
}

static void ml_raise_gerror_exn(GError *, value *) Noreturn;
static void ml_raise_gerror_exn(GError *err, value *exn)
{
  CAMLunused CAMLparam0();
  CAMLlocal2(b, msg);
  g_assert (err && exn);
  msg = copy_string(err->message);
  b = alloc_small (3, 0);
  Field (b, 0) = *exn;
  Field (b, 1) = Val_int(err->code);
  Field (b, 2) = msg;
  g_error_free (err);
  mlraise(b);
}

static void ml_raise_generic_gerror (GError *) Noreturn;
static void ml_raise_generic_gerror (GError *err)
{
  static const value *exn;
  value msg;
  if (exn == NULL) {
    exn = caml_named_value ("gerror");
    if (exn == NULL)
      failwith ("gerror");
  }
  msg = copy_string (err->message);
  g_error_free (err);
  raise_with_arg (*exn, msg);
}

CAMLprim void ml_raise_gerror(GError *err)
{
  value *caml_exn;
  g_assert (err);
  caml_exn = lookup_exn_map (err->domain);
  if (caml_exn)
    ml_raise_gerror_exn (err, caml_exn);
  else 
    ml_raise_generic_gerror (err);
}

/* Main loop handling */

/* for 1.3 compatibility */
#ifdef g_main_new
#undef g_main_new
#define	g_main_new(is_running)	g_main_loop_new (NULL, is_running)
#endif

/* More compatibility changes to avoid deprecation warnings: these just
   replicate the compatibility definitions from glib/deprecated/gmain.h,
   but without the warnings. */
#ifdef g_main_iteration
#undef g_main_iteration
#define g_main_iteration(may_block) g_main_context_iteration(NULL, may_block)
#endif

#ifdef g_main_pending
#undef g_main_pending
#define g_main_pending() g_main_context_pending(NULL)
#endif

#ifdef g_main_is_running
#undef g_main_is_running
#define g_main_is_running(loop) g_main_loop_is_running(loop)
#endif

#ifdef g_main_quit
#undef g_main_quit
#define g_main_quit(loop) g_main_loop_quit(loop)
#endif

#define GMainLoop_val(val) ((GMainLoop *) Addr_val(val))
ML_1 (g_main_new, Bool_val, Val_addr)
ML_1 (g_main_iteration, Bool_val, Val_bool)
ML_0 (g_main_pending, Val_bool)
ML_1 (g_main_is_running, GMainLoop_val, Val_bool)
ML_1 (g_main_quit, GMainLoop_val, Unit)

static gboolean ml_g_source_func (gpointer data)
{
  value res, *clos = data;
  res = callback_exn (*clos, Val_unit);
  if (Is_exception_result(res))
    {
      CAML_EXN_LOG ("GSourceFunc");
      return FALSE;
    }
  return Bool_val (res);
}

CAMLprim value ml_g_timeout_add (value o_prio, value interval, value clos)
{
    value *clos_p = ml_global_root_new (clos);
    return Val_int
        (g_timeout_add_full (Option_val(o_prio, Int_val, G_PRIORITY_DEFAULT),
			     Long_val(interval),
                             ml_g_source_func, clos_p,
                             ml_global_root_destroy));
}

CAMLprim value ml_g_idle_add (value o_prio, value clos)
{
    value *clos_p = ml_global_root_new (clos);
    return Val_int
      (g_idle_add_full (Option_val(o_prio, Int_val, G_PRIORITY_DEFAULT_IDLE),
			ml_g_source_func, clos_p,
			ml_global_root_destroy));
}

ML_1 (g_source_remove, Int_val, Unit)

/* single-linked lists */

CAMLprim value Val_GSList (GSList *list, value (*func)(gpointer))
{
  CAMLparam0();
  CAMLlocal4 (new_cell, result, last_cell, cell);
  
  last_cell = cell = Val_unit;
  while (list != NULL) {
    result = func(list->data);
    new_cell = alloc_small(2,0);
    Field(new_cell,0) = result;
    Field(new_cell,1) = Val_unit;
    if (last_cell == Val_unit) cell = new_cell;
    else modify(&Field(last_cell,1), new_cell);
    last_cell = new_cell;
    list = list->next;
  }
  CAMLreturn(cell);
}

CAMLprim value Val_GSList_free (GSList *list, value (*func)(gpointer))
{
  value res = Val_GSList (list, func);
  g_slist_free (list);
  return res;
}

CAMLprim GSList *GSList_val (value list, gpointer (*func)(value))
{
    GSList *res = NULL;
    GSList **current = &res;
    value cell = list;
    while (Is_block(cell)) {
	*current = g_slist_alloc ();
	(*current)->data = func(Field(cell,0));
	cell = Field(cell,1);
	current = &(*current)->next;
    }
    return res;
}

ML_0 (g_get_prgname, copy_string_or_null)
ML_1 (g_set_prgname, String_val, Unit)
#ifdef HASGTK22
ML_0 (g_get_application_name, copy_string_or_null)
ML_1 (g_set_application_name, String_val, Unit)
#else
Unsupported(g_get_application_name)
Unsupported(g_set_application_name)
#endif

ML_0 (g_get_user_name, copy_string)
ML_0 (g_get_real_name, copy_string)
CAMLprim value ml_g_get_home_dir (value unit)
{
  const char *s = g_get_home_dir();
  return s ? ml_some (copy_string (s)) : Val_unit;
}
ML_0 (g_get_tmp_dir, copy_string)
CAMLprim value ml_g_find_program_in_path (value p)
{
  value v;
  char *s = g_find_program_in_path (String_val(p));
  if (s == NULL) raise_not_found();
  v = copy_string(s);
  g_free(s);
  return v;
}

CAMLprim value ml_g_getenv (value v)
{
  const gchar *s = g_getenv(String_val(v));
  if (s == NULL) raise_not_found();
  return copy_string(s);
}

#ifdef HASGTK24
CAMLprim value ml_g_setenv (value v, value s, value o)
{
  if (! g_setenv(String_val(v), String_val(s), Bool_val(o)))
    failwith("g_setenv");
  return Val_unit;
}
ML_1 (g_unsetenv, String_val, Unit)
#else
Unsupported_24(g_setenv)
Unsupported_24(g_unsetenv)
#endif /* HASGTK24 */

#ifdef HASGTK26
ML_0 (g_get_user_cache_dir, copy_string)
ML_0 (g_get_user_data_dir, copy_string)
ML_0 (g_get_user_config_dir, copy_string)
ML_0 (g_get_system_data_dirs, copy_string_v)
ML_0 (g_get_system_config_dirs, copy_string_v)
#else
Unsupported_26(g_get_user_cache_dir)
Unsupported_26(g_get_user_data_dir)
Unsupported_26(g_get_user_config_dir)
Unsupported_26(g_get_system_data_dirs)
Unsupported_26(g_get_system_config_dirs)
#endif /* HASGTK26 */

ML_1(g_usleep,Long_val,Unit)
